use std::collections::{btree_map, BTreeMap, BTreeSet};

use hubris_build_kconfig as kconfig;
use miette::IntoDiagnostic as _;

use crate::{appcfg::AppDef, relink::BuiltTask};

/// Generates kernel configuration for a given build of an app.
///
/// `app` gives the static appcfg, while `built_tasks` describes the actual
/// build output, including allocation decisions and sizes.
///
/// This function does not access the filesystem or parse ELF files; everything
/// it needs to know is in its arguments.
pub fn generate_kconfig(
    app: &AppDef,
    built_tasks: &[BuiltTask],
) -> miette::Result<kconfig::KernelConfig> {
    let shared_regions = app.board.chip.peripherals.iter()
        .map(|(name, pdef)| {
            let pdef = pdef.value();

            (name.clone(), kconfig::RegionConfig {
                base: *pdef.base.value() as u32,
                size: *pdef.size.value() as u32,
                attributes: kconfig::RegionAttributes {
                    read: true,
                    write: true,
                    execute: false,
                    special_role: Some(kconfig::SpecialRole::Device),
                },
            })
        })
    .collect();

    let mut kconfig = kconfig::KernelConfig {
        tasks: vec![],
        shared_regions,
        irqs: BTreeMap::new(),
    };
    for (i, task) in app.tasks.values().enumerate() {
        for (pname, puse) in &task.peripherals {
            let periphdef = &app.board.chip.peripherals[pname];
            let interrupts = &periphdef.value().interrupts;

            for (pirqname, notname) in &puse.value().interrupts {
                let irqnum = interrupts[pirqname];

                match kconfig.irqs.entry(irqnum) {
                    btree_map::Entry::Vacant(v) => {
                        v.insert(kconfig::InterruptConfig {
                            task_index: i,
                            notification: 1 << task.notifications.get_index_of(notname).unwrap(),
                        });
                    }
                    btree_map::Entry::Occupied(_) => {
                        panic!("internal inconsistency: interrupt {irqnum} defined in multiple places");
                    }
                }
            }
        }
    }

    let mut used_shared_regions = BTreeSet::new();

    for task in built_tasks {
        let mut config = kconfig::TaskConfig {
            owned_regions: BTreeMap::new(),
            shared_regions: BTreeSet::new(),
            entry_point: kconfig::OwnedAddress {
                region_name: task.entry.region.clone(),
                offset: u32::try_from(task.entry.offset).into_diagnostic()?,
            },
            initial_stack: kconfig::OwnedAddress {
                region_name: task.initial_stack_pointer.region.clone(),
                offset: u32::try_from(task.initial_stack_pointer.offset).into_diagnostic()?,
            },
            priority: *app.tasks[&task.name].priority.value(),
            start_at_boot: !app.tasks[&task.name].wait_for_reinit,
        };

        for (name, reg) in &task.owned_regions {
            let mem = &app.board.chip.memory[name].value();

            config.owned_regions.insert(name.clone(), kconfig::MultiRegionConfig {
                base: u32::try_from(reg.base).into_diagnostic()?,
                sizes: reg.sizes.iter().map(|n| u32::try_from(*n)
                    .into_diagnostic())
                    .collect::<miette::Result<Vec<_>>>()?,
                    attributes: kconfig::RegionAttributes {
                        read: mem.read,
                        write: mem.write,
                        execute: mem.execute,
                        special_role: None, // TODO
                    },
            });
        }

        for name in app.tasks[&task.name].peripherals.keys() {
            used_shared_regions.insert(name);
            config.shared_regions.insert(name.clone());
        }

        kconfig.tasks.push(config);
    }

    kconfig.shared_regions.retain(|name, _| used_shared_regions.contains(name));

    Ok(kconfig)
}
