use std::{collections::BTreeMap, ops::{RangeInclusive, Range}};
use indexmap::IndexMap;
use hubris_region_alloc::{Mem, TaskInfo, TaskName};
use miette::bail;

use crate::{appcfg::{KernelDef, RegionDef, Spanned}, SizeRule, TargetSpec};

#[derive(Clone, Debug, Default)]
pub struct Allocations {
    pub tasks: IndexMap<String, BTreeMap<String, TaskAllocation>>,
    pub kernel: KernelAllocation,
}

impl Allocations {
    pub fn by_region(&self) -> BTreeMap<&str, IndexMap<&str, &TaskAllocation>> {
        let mut pivot: BTreeMap<_, IndexMap<_, _>> = BTreeMap::new();
        for (task_name, ta) in &self.tasks {
            for (region_name, alloc) in ta {
                pivot.entry(region_name.as_str())
                    .or_default()
                    .insert(task_name.as_str(), alloc);
            }
        }
        pivot
    }
}

#[derive(Clone, Debug)]
pub struct KernelAllocation {
    pub by_region: BTreeMap<Mem, Range<u64>>,
    pub stack: Range<u64>,
}

impl Default for KernelAllocation {
    fn default() -> Self {
        Self { by_region: Default::default(), stack: 0..0 }
    }
}

#[derive(Clone, Debug)]
pub struct TaskAllocation {
    pub requested: u64,
    pub base: u64,
    pub sizes: Vec<u64>,
}

impl TaskAllocation {
    pub fn calculate_size(&self) -> u64 {
        self.sizes.iter().sum()
    }

    pub fn range_inclusive(&self) -> RangeInclusive<u64> {
        let size = self.calculate_size();
        self.base..=self.base + (size - 1)
    }
}

pub fn allocate_space(
    target_spec: &TargetSpec,
    available: &IndexMap<String, Spanned<RegionDef>>,
    tasks: &BTreeMap<TaskName, TaskInfo>,
    kernel: &KernelDef,
) -> miette::Result<Allocations> {
    let mut available = available.iter().map(|(name, def)| {
        let base = *def.value().base.value();
        (Mem(name.clone()), base..base + def.value().size.value())
    }).collect::<BTreeMap<_, _>>();
    let mut results = Allocations::default();
    for (region_name, region) in &mut available {
        let is_ram = region_name.0.eq_ignore_ascii_case("RAM");
        // Derive some region characteristics.
        let orig_addr = region.start;
        let mut addr = target_spec.align_before_allocation(orig_addr);
        // Carve stack space out of the RAM region.
        if is_ram {
            addr = target_spec.align_for_stack(addr);
            results.kernel.stack = addr..addr + kernel.stack_size.value();
            addr += kernel.stack_size.value();
            addr = target_spec.align_for_stack(addr);
        }
        if orig_addr != addr {
            eprintln!("warning: adjusted region {region_name} base up to {addr:#x} from {orig_addr:#x}");
        }

        region.start = addr;
    }

    match &target_spec.size_rule {
        SizeRule::PowerOfTwo => {
            let granule = target_spec.alloc_minimum.next_power_of_two();
            let round = granule - 1;
            available.values_mut().for_each(|range| {
                range.start = (range.start + round) / granule;
                range.end = (range.end + round) / granule;
            });
            let tasks_granule = tasks.iter().map(|(name, info)| {
                (name.clone(), TaskInfo {
                    regs_avail: info.regs_avail,
                    reqs: info.reqs.iter()
                        .map(|(mem, size)| (mem.clone(), (size + round) / granule))
                        .collect(),
                })
            }).collect::<BTreeMap<_, _>>();
            let Some((aresult, leftover)) = hubris_region_alloc::allocate(&tasks_granule, &available) else {
                bail!("can't fit app in memory");
            };
            for (name, info) in tasks {
                let ta = results.tasks.entry(name.0.clone()).or_default();
                for (region, alloc) in &aresult.tasks[name].regions {
                    ta.insert(region.0.clone(), TaskAllocation {
                        requested: info.reqs[region],
                        base: alloc.1 * granule,
                        sizes: alloc.2.iter().map(|i| (1 << i) * granule).collect(),
                    });
                }

            }
            results.kernel.by_region = leftover.into_iter().map(|(name, range)| {
                (name, range.start * granule .. range.end * granule)
            }).collect();
        }
        SizeRule::MultipleOf(_granule) => {
            todo!()
        }
    }

    Ok(results)
}


