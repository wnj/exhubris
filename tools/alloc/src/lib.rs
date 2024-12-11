//! Memory region allocator for pow2-aligned MPUs.

use std::{cmp::Ordering, collections::BTreeMap, fmt::Display, ops::Range};

#[derive(Clone, Debug, Default, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Mem(pub String);

impl Display for Mem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Clone, Debug, Default, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct TaskName(pub String);

impl Display for TaskName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Clone, Debug, Default)]
pub struct TaskInfo {
    pub regs_avail: usize,
    pub reqs: BTreeMap<Mem, u64>,
}

pub fn split_task(
    task: &TaskInfo,
    mem_sizes: &BTreeMap<Mem, u64>,
) -> SplitTask {
    let mut regs: BTreeMap<Mem, [u8; 64]> = task.reqs.iter().map(|(memname, reqsize)| {
        (memname.clone(), core::array::from_fn(|i| (reqsize & (1 << i) != 0) as u8))
    }).collect();

    loop {
        // How many regs are in use?
        let in_use = regs.values()
            .flat_map(|digits| digits.iter().map(|&i| i as usize))
            .sum::<usize>();
        match in_use.cmp(&task.regs_avail) {
            Ordering::Less => {
                // We could split to make allocation easier. Choose based on
                // percentage of memory.
                let candidates = regs.iter()
                    .filter(|(_name, digits)| digits.iter().map(|&i| i as usize).sum::<usize>() > 0)
                    .flat_map(|(memname, digits)| {
                        digits.iter().rposition(|&n| n > 0)
                            .map(|i| (memname.clone(), i, mem_sizes[memname] / (1 << i)))
                    })
                    .collect::<Vec<_>>();
                let (memname, bit_pos, _) = candidates.into_iter().min_by_key(|(_name, _i, factor)| *factor).unwrap();
                if bit_pos == 0 {
                    break;
                }
                let digits = regs.get_mut(&memname).unwrap();
                digits[bit_pos] -= 1;
                digits[bit_pos - 1] += 2;
            }
            Ordering::Greater => {
                // We must merge, we are out of regions. Choose based on what
                // percentage of the memory space we're potentially losing, and
                // exempt any memories that are already down to one region.
                let candidates = regs.iter()
                    .filter(|(_name, digits)| digits.iter().map(|&i| i as usize).sum::<usize>() > 1)
                    .flat_map(|(memname, digits)| {
                        digits.iter().position(|&n| n > 0)
                            .map(|i| (memname.clone(), i, mem_sizes[memname] / (1 << i)))
                    })
                    .collect::<Vec<_>>();
                let (memname, bit_pos, _) = candidates.into_iter().max_by_key(|(_name, _i, factor)| *factor).unwrap();
                // bit_pos is the l2size of the victim region we've identified.
                // The victim region will be eliminated. We have two potential
                // ways of doing this:
                // - If there are more than one region in that l2size, we can
                //   merge them, eliminating two regions and creating one of the
                //   next size up.
                // - If not, then we must combine with the next size up.
                let digits = regs.get_mut(&memname).unwrap();
                digits[bit_pos] -= 1;
                let next_size = digits[bit_pos..].iter()
                    .position(|&n| n != 0)
                    .unwrap() + bit_pos;
                digits[next_size] -= 1;
                digits[next_size + 1] += 1;
            }
            Ordering::Equal => {
                break;
            }
        }
    }

    // Filter out empty requests.
    regs.retain(|_, digits| digits.iter().any(|&n| n != 0));

    SplitTask { regs }
}

#[derive(Clone, Debug, Default)]
pub struct SplitTask {
    pub regs: BTreeMap<Mem, [u8; 64]>,
}


pub fn allocate(
    tasks: &BTreeMap<TaskName, TaskInfo>,
    mem_avail: &BTreeMap<Mem, Range<u64>>,
) -> Option<(TaskAllocation, BTreeMap<Mem, Range<u64>>)> {
    // Compute available memory sizes for assessing costs.
    let mem_sizes = mem_avail.iter().map(|(name, range)| {
        (name.clone(), range.end - range.start)
    }).collect::<BTreeMap<Mem, u64>>();

    // Compute task tiling.
    let mut tiling: BTreeMap<(TaskName, Mem), [u8; 64]> = tasks.iter()
        .flat_map(|(name, info)| split_task(info, &mem_sizes).regs
            .into_iter()
            .map(|(mem, digits)| ((name.clone(), mem), digits)))
        .collect();

    let mut result = TaskAllocation::default();
    let mut mem_avail = mem_avail.clone();

    while !tiling.is_empty() {
        // Figure out who can be placed 
        let candidate = tiling.iter()
            .filter_map(|((task, mem), digits)| {
                place_task(digits, mem_avail[mem].clone())
                    .map(|(pad, order, left)| (task, mem, pad, mem_sizes[mem].checked_div(pad).unwrap_or(u64::MAX), order, left))
            })
            .max_by_key(|(_task, _mem, _pad, cost, ..)| *cost);
        let Some((c_task, c_mem, c_pad, _cost, c_order, c_left)) = candidate else {
            panic!("cannot place a task here even with padding");
        };

        *result.waste.entry(c_mem.clone()).or_default() += c_pad;
        let ta = result.tasks.entry(c_task.clone()).or_default();
        let padded_addr = mem_avail[c_mem].start + c_pad;

        ta.regions.insert(c_mem.clone(), (c_pad, padded_addr, c_order));

        mem_avail.insert(c_mem.clone(), c_left);

        tiling.remove(&(c_task.clone(), c_mem.clone()));
    }

    Some((result, mem_avail))
}

fn place_task(
    digits: &[u8; 64],
    orig_avail: Range<u64>,
) -> Option<(u64, Vec<usize>, Range<u64>)> {
    // Construct the digit occupancy bitset.
    let occupancy: u64 = digits.iter().enumerate()
        .fold(0, |mask, (i, count)| if *count > 0 {
            mask | (1 << i)
        } else {
            mask
        });
    assert!(occupancy != 0, "attempt to place an empty task??");

    let pad_unit = 1 << occupancy.trailing_zeros();
    let mut avail = orig_avail.clone();
    avail.start = avail.start.next_multiple_of(pad_unit);

    'alignloop:
    loop {
        let padding_used = avail.start - orig_avail.start;
        // See if things actually fit.
        let mut digits_left = *digits;
        let mut used_order = vec![];
        while digits_left.iter().any(|&n| n > 0) {
            if avail.is_empty() {
                // We have run out of space.
                return None;
            }
            let addr_alignment = avail.start.trailing_zeros() as usize;
            // Find the largest nonzero digit that fits here.
            let next_digit = if addr_alignment == 64 {
                digits_left.iter().rposition(|&n| n > 0)
            } else {
                digits_left[..=addr_alignment].iter()
                    .rposition(|&n| n > 0)
            };
            let Some(next_digit) = next_digit else {
                avail.start += pad_unit;
                continue 'alignloop;
            };
            digits_left[next_digit] -= 1;
            avail.start += 1 << next_digit;
            used_order.push(next_digit);
        }

        return Some((padding_used, used_order, avail));
    }
}

#[derive(Clone, Default, Debug)]
pub struct TaskAllocation {
    pub tasks: BTreeMap<TaskName, TaskAlloc>,
    pub waste: BTreeMap<Mem, u64>,
}

#[derive(Clone, Default, Debug)]
pub struct TaskAlloc {
    pub regions: BTreeMap<Mem, (u64, u64, Vec<usize>)>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn split_task_basic() {
        let flash = "flash".to_string();
        let ram = "ram".to_string();
        let tasks = [
            (TaskName("super".to_string()), TaskInfo {
                regs_avail: 7,
                reqs: [
                    (Mem(flash.clone()), 8),
                    (Mem(ram.clone()), 8),
                ].into_iter().collect(),
            }),
            (TaskName("idle".to_string()), TaskInfo {
                regs_avail: 7,
                reqs: [
                    (Mem(flash.clone()), 3),
                    (Mem(ram.clone()), 4),
                ].into_iter().collect(),
            }),
            (TaskName("scanner".to_string()), TaskInfo {
                regs_avail: 7,
                reqs: [
                    (Mem(flash.clone()), 49),
                    (Mem(ram.clone()), 12),
                ].into_iter().collect(),
            }),
            (TaskName("usbhid".to_string()), TaskInfo {
                regs_avail: 6,
                reqs: [
                    (Mem(flash.clone()), 106),
                    (Mem(ram.clone()), 43),
                ].into_iter().collect(),
            }),
            (TaskName("keyboard".to_string()), TaskInfo {
                regs_avail: 7,
                reqs: [
                    (Mem(flash.clone()), 38),
                    (Mem(ram.clone()), 12),
                ].into_iter().collect(),
            }),
            (TaskName("sys".to_string()), TaskInfo {
                regs_avail: 5,
                reqs: [
                    (Mem(flash.clone()), 38),
                    (Mem(ram.clone()), 7),
                ].into_iter().collect(),
            }),
        ].into_iter().collect::<BTreeMap<_, _>>();

        let mem_avail = [
            (Mem(flash.clone()), 0x0800_0200>>5..0x0802_0000>>5),
            (Mem(ram.clone()), 0x2000_0280>>5..0x2000_a000>>5),
        ].into_iter().collect::<BTreeMap<_, _>>();

        let mem_sizes = mem_avail.iter().map(|(name, range)| {
            (name.clone(), range.end - range.start)
        }).collect::<BTreeMap<Mem, u64>>();

        for (name, info) in tasks {
            let split = split_task(&info, &mem_sizes);
            let mut regions_used = 0;
            for (memname, splits) in split.regs {
                let orig = info.reqs[&memname];
                let mut actual_size = 0;
                for (i, n) in splits.iter().enumerate() {
                    if *n != 0 {
                        actual_size += *n as u64 * (1 << i);
                        regions_used += *n as usize;
                    }
                }
                assert!(actual_size >= orig,
                    "{name} allocation is smaller than requested");
            }
            assert!(regions_used <= info.regs_avail,
                "{name} too many regions are used");
        }
    }

    #[test]
    fn allocate_example_app() {
        let flash = Mem("flash".to_string());
        let ram = Mem("ram".to_string());
        let tasks = [
            (TaskName("super".to_string()), TaskInfo {
                regs_avail: 7,
                reqs: [
                    (flash.clone(), 8),
                    (ram.clone(), 8),
                ].into_iter().collect(),
            }),
            (TaskName("idle".to_string()), TaskInfo {
                regs_avail: 7,
                reqs: [
                    (flash.clone(), 3),
                    (ram.clone(), 4),
                ].into_iter().collect(),
            }),
            (TaskName("scanner".to_string()), TaskInfo {
                regs_avail: 7,
                reqs: [
                    (flash.clone(), 49),
                    (ram.clone(), 12),
                ].into_iter().collect(),
            }),
            (TaskName("usbhid".to_string()), TaskInfo {
                regs_avail: 6,
                reqs: [
                    (flash.clone(), 106),
                    (ram.clone(), 43),
                ].into_iter().collect(),
            }),
            (TaskName("keyboard".to_string()), TaskInfo {
                regs_avail: 7,
                reqs: [
                    (flash.clone(), 38),
                    (ram.clone(), 12),
                ].into_iter().collect(),
            }),
            (TaskName("sys".to_string()), TaskInfo {
                regs_avail: 5,
                reqs: [
                    (flash.clone(), 38),
                    (ram.clone(), 7),
                ].into_iter().collect(),
            }),
        ].into_iter().collect();

        let mem_avail = [
            (flash.clone(), 0x0800_0200>>5..0x0802_0000>>5),
            (ram.clone(), 0x2000_0280>>5..0x2000_a000>>5),
        ].into_iter().collect();

        let (solution, _leftover) = allocate(
            &tasks,
            &mem_avail,
        ).expect("this should be possible");
       
        let mut rows = vec![];
        for (taskname, taskalloc) in solution.tasks {
            for (memname, (pad_before, start, sizes)) in taskalloc.regions {
                let sizes: Vec<_> = sizes.into_iter().map(|i| (1 << (5 + i)).to_string()).collect();
                rows.push((
                    taskname.0.clone(),
                    memname.0.to_string(),
                    pad_before << 5,
                    start << 5,
                    sizes.join(" "),
                ));
            }
        }
        rows.sort_by(|a, b| a.3.cmp(&b.3));
        let mut table = comfy_table::Table::new();
        table.load_preset(comfy_table::presets::UTF8_FULL);
        for row in rows {
            table.add_row([
                row.0,
                row.1,
                row.2.to_string(),
                format!("{:#x}", row.3),
                row.4,
            ]);
        }
        println!("{table}");
        println!("waste:");
        let mut table = comfy_table::Table::new();
        table.load_preset(comfy_table::presets::UTF8_FULL);
        for (name, amt) in solution.waste {
            let amt = amt << 5;
            table.add_row([
                name.0.to_string(),
                format!("{amt}"),
            ]);
        }
        println!("{table}");
    }

    #[test]
    fn place_task_basic() {
        let avail = 0..1024;

        let mut digits = [0; 64];
        digits[3] = 1;

        let (padding, order, remaining) = place_task(&digits, avail.clone())
            .expect("allocation should succeed");
        assert_eq!(padding, 0);
        assert_eq!(remaining.start, 1 << 3);
        assert_eq!(remaining.end, avail.end);
        assert_eq!(order, [3]);
    }
    #[test]
    fn place_task_harder() {
        let avail = 512..8192;

        let mut digits = [0; 64];
        digits[9] = 1;
        digits[11] = 1;

        let (padding, order, remaining) = place_task(&digits, avail.clone())
            .expect("allocation should succeed");
        assert_eq!(padding, 1024);
        assert_eq!(remaining.start, 4096);
        assert_eq!(remaining.end, avail.end);
        assert_eq!(order, [9, 11]);
    }
}
