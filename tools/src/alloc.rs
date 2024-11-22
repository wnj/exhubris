use std::{collections::BTreeMap, ops::RangeInclusive};

use indexmap::IndexMap;

use crate::{appcfg::{KernelDef, RegionDef, Spanned}, TargetSpec};

#[derive(Clone, Debug, Default)]
pub struct Allocations {
    pub tasks: IndexMap<String, BTreeMap<String, TaskAllocation>>,
    pub kernel: KernelAllocation,
}

impl Allocations {
    pub fn by_region(&self) -> BTreeMap<&str, IndexMap<&str, &TaskAllocation>> {
        let mut pivot: BTreeMap<_, IndexMap<_, _>> = BTreeMap::new();
        for (task_name, regions) in &self.tasks {
            for (region_name, alloc) in regions {
                pivot.entry(region_name.as_str())
                    .or_default()
                    .insert(task_name.as_str(), alloc);
            }
        }
        pivot
    }
}

#[derive(Clone, Debug)]
pub struct TaskAllocation {
    pub requested: u64,
    pub actual: RangeInclusive<u64>,
}

#[derive(Clone, Debug)]
pub struct KernelAllocation {
    pub by_region: BTreeMap<String, RangeInclusive<u64>>,
    pub stack: RangeInclusive<u64>,
}

impl Default for KernelAllocation {
    fn default() -> Self {
        Self { by_region: Default::default(), stack: 0..=0 }
    }
}

pub fn allocate_space(
    target_spec: &TargetSpec,
    available: &IndexMap<String, Spanned<RegionDef>>,
    required: &BTreeMap<String, IndexMap<&str, u64>>,
    kernel: &KernelDef,
) -> miette::Result<Allocations> {
    let mut results = Allocations::default();

    for (region_name, region) in available {
        let Some(reqs) = required.get(region_name.as_str()) else {
            continue;
        };
        let is_ram = region_name.eq_ignore_ascii_case("RAM");
        // Sort requests for this region in descending size order, under the
        // assumption that larger requests also have larger alignment
        // requirements (on platforms where such alignment requirements exist).
        // TODO: this is merely an approximation of the right approach.
        let mut reqs = reqs.iter().collect::<Vec<_>>();
        reqs.sort_by_key(|(_name, size)| *size);
        reqs.reverse();

        // Derive some region characteristics.
        let orig_addr = *region.value().base.value();
        let end = orig_addr + *region.value().size.value();
        let mut addr = target_spec.align_before_allocation(orig_addr);
        // Carve stack space out of the RAM region.
        if is_ram {
            addr = target_spec.align_for_stack(addr);
            results.kernel.stack = addr..=addr + (kernel.stack_size.value() - 1);
            addr += kernel.stack_size.value();
            addr = target_spec.align_for_stack(addr);
        }
        if orig_addr != addr {
            eprintln!("warning: adjusted region {region_name} base up to {addr:#x} from {orig_addr:#x}");
        }

        // Filter out zero-size requests.
        reqs.retain(|(_name, size)| **size != 0);

        // Attempt to allocate in multiple passes, inserting padding if
        // required.
        while !reqs.is_empty() {
            // Attempt to satisfy each request without increasing the alignment.
            let start_len = reqs.len();
            reqs.retain(|(taskname, requested_size)| {
                let size = target_spec.round_allocation_size(**requested_size);
                let reqd_addr = target_spec.align_for_allocation_size(addr, size);
                if reqd_addr == addr {
                    // Sweet, we can satisfy this without padding.
                    results.tasks.entry(taskname.to_string())
                        .or_default()
                        .insert(region_name.clone(), TaskAllocation {
                            requested: **requested_size,
                            actual: addr..=addr + (size - 1),
                        });
                    addr += size;
                    false
                } else {
                    // Keep this for the next pass.
                    true
                }
            });

            if !reqs.is_empty() && reqs.len() == start_len {
                addr = target_spec.align_to_next_larger_boundary(addr);
            }
        }

        results.kernel.by_region.insert(region_name.to_string(), addr..=end - 1);
    }

    Ok(results)
}


