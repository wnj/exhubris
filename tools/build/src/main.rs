use std::{collections::{btree_map, BTreeMap, BTreeSet}, fs, io::{ErrorKind, Write as _}, path::{Path, PathBuf}, sync::Arc, time::Instant};

use clap::Parser;
use comfy_table::CellAlignment;
use miette::{miette, Context, IntoDiagnostic as _, LabeledSpan, NamedSource};
use rangemap::RangeMap;
use size::Size;
use hubris_build::{alloc::allocate_space, appcfg, buildid::BuildId, cargo::{do_cargo_build, LinkStyle}, get_target_spec, relink::{relink_final, relink_for_size}, verbose::{banner, simple_table}};
use hubris_region_alloc::{Mem, TaskInfo, TaskName};
use hubris_build_kconfig as kconfig;

#[derive(Parser)]
struct Tool {
    #[clap(subcommand)]
    cmd: Cmd,
}

#[derive(Parser)]
enum Cmd {
    /// Builds a Hubris application, given a path to its config.
    Build {
        /// Path to the application definition.
        cfg_path: PathBuf,

        /// Cause Cargo to become very chatty (passes the `--verbose` flag to
        /// all Cargo commands)
        #[clap(long)]
        cargo_verbose: bool,

        /// Overrides the output bundle path, if you'd like it placed somewhere
        /// specific. Defaults to APPNAME-build.zip in the project root.
        #[clap(short, long)]
        out: Option<PathBuf>,
    },
    PackHex {
        bindir: PathBuf,
        outpath: PathBuf,

        #[clap(short, long)]
        gdbconfig: Option<PathBuf>,
    },
    CheckIdl {
        path: PathBuf,
    },
    Bundle {
        cfg_path: PathBuf,
        bindir: PathBuf,
        outpath: PathBuf,
    },
}

fn main() -> miette::Result<()> {
    let args = Tool::parse();

    match args.cmd {
        Cmd::Build { cfg_path, cargo_verbose, out } => {
            // Canonicalize directories and locate/parse input files.
            let root = std::env::var("HUBRIS_PROJECT_ROOT").into_diagnostic()?;
            let root = PathBuf::from(root);
            let doc_src = fs::read_to_string(&cfg_path)
                .into_diagnostic()
                .wrap_err_with(|| format!("can't read {}", cfg_path.display()))?;
            let source = Arc::new(NamedSource::new(
                cfg_path.display().to_string(),
                doc_src.clone(),
            ));
            let doc: kdl::KdlDocument = appcfg::add_source(&source, || {
                Ok(doc_src.parse()?)
            })?;
            let ctx = appcfg::FsContext::from_root(&root);
            let app = appcfg::parse_app(source, &doc, &ctx)?;

            let mut buildid = BuildId::new();

            // See if we understand this target.
            let target_spec = get_target_spec(app.board.chip.target_triple.value())
                .ok_or_else(|| {
                    miette!(
                        labels = [LabeledSpan::at(
                            app.board.chip.target_triple.span(),
                            "triple defined here",
                        )],
                        "target spec not defined for triple"
                    )
                })?;
            buildid.hash(&target_spec);

            // Interrogate the installed toolchain to discover things like the
            // linker path.
            let env = hubris_build::determine_build_env()?;

            buildid.hash(&env);

            // Print the kickoff header:
            simple_table([
                ("App name", app.name.value().as_str()),
                ("Config path", &cfg_path.display().to_string()),
                ("Workspace root", &root.display().to_string()),
                ("Host platform", &env.host_triple),
                ("Default toolchain", &env.release),
            ]);

            // Analyze the app and make a build plan.
            let overall_plan = hubris_build::appcfg::plan_build(&app)?;
            // Locate the workspace Cargo target directory.
            let targetroot = root.join("target");
            // Create our working directory.
            let workdir = root.join(".work").join(app.name.value());
            maybe_create_dir(&workdir).into_diagnostic()?;

            // Create our tmp directory, which we'll use for short-lived files
            // during the build process.
            let tmpdir = workdir.join("tmp");
            maybe_create_dir(&tmpdir).into_diagnostic()?;

            // Create our initial build directory, where we deposit
            // partially-linked task binaries.
            let initial_build_dir = workdir.join("build");
            maybe_create_dir(&initial_build_dir).into_diagnostic()?;
            let task_rlink_text = include_str!("../../../files/task-rlink.x");
            buildid.eat(task_rlink_text.as_bytes());
            for (name, plan) in &overall_plan.tasks {
                do_cargo_build(
                    task_rlink_text,
                    plan,
                    LinkStyle::Partial,
                    &targetroot,
                    &tmpdir,
                    &initial_build_dir,
                    name,
                    cargo_verbose,
                )?;
            }

            // Begin the second link phase...
            banner("Task build complete, prelinking for size...");

            let mut size_reqs: BTreeMap<TaskName, TaskInfo> = BTreeMap::new();
            let temp_link_dir = workdir.join("link2");
            maybe_create_dir(&temp_link_dir).into_diagnostic()?;
            let task_link2_text = include_str!("../../../files/task-link2.x");
            buildid.eat(task_link2_text.as_bytes());
            std::fs::write(workdir.join("task-link2.x"), task_link2_text).into_diagnostic()?;
            for taskname in overall_plan.tasks.keys() {
                let region_sizes = relink_for_size(
                    &env,
                    &app,
                    &target_spec,
                    &targetroot,
                    &app.tasks[taskname],
                    &initial_build_dir.join(taskname),
                    &temp_link_dir.join(taskname),
                    &workdir.join("task-link2.x"),
                )?;

                let ti = size_reqs.entry(TaskName(taskname.clone())).or_default();
                for (region, range) in region_sizes {
                    let size = range.end - range.start;
                    ti.reqs.insert(Mem(region), size);
                }
                ti.regs_avail = target_spec.region_count - app.tasks[taskname].peripherals.len();
            }
            {
                let mut table = comfy_table::Table::new();
                table.load_preset(comfy_table::presets::NOTHING);
                table.set_header(["REGION", "OWNER", "SIZE"]);

                for (taskname, ti) in &size_reqs {
                    for (memname, size) in &ti.reqs {
                        table.add_row([taskname.to_string(), memname.to_string(), Size::from_bytes(*size).to_string()]);
                    }
                }

                println!("{table}");
                println!();
            }

            let alloc_begin = Instant::now();
            let allocs = allocate_space(&target_spec, &app.board.chip.memory, &size_reqs, &app.kernel)?;
            let alloc_time = alloc_begin.elapsed();

            buildid.hash(&allocs);

            println!("Allocations ({alloc_time:?}):");

            let mut table = comfy_table::Table::new();
            table.load_preset(comfy_table::presets::NOTHING);
            table.set_header(["MEMORY", "TASK", "START", "END", "SIZE", "LOST"]);
            table.column_mut(2).unwrap().set_cell_alignment(CellAlignment::Right);
            table.column_mut(3).unwrap().set_cell_alignment(CellAlignment::Right);
            table.column_mut(4).unwrap().set_cell_alignment(CellAlignment::Right);
            table.column_mut(5).unwrap().set_cell_alignment(CellAlignment::Right);
            for (region_name, regallocs) in allocs.by_region() {
                let mut regallocs = regallocs.iter().collect::<Vec<_>>();
                regallocs.sort_by_key(|(_name, ta)| ta.base);
                let mut total = 0;
                let mut loss = 0;

                let mut last = None;
                for (task_name, talloc) in regallocs {
                    let base = talloc.base;
                    let req_size = talloc.requested;

                    if let Some(last) = last {
                        if base != last {
                            let pad_size = base - last;
                            total += pad_size;
                            loss += pad_size;
                            table.add_row([
                                region_name.to_string(),
                                "-pad-".to_string(),
                                format!("{:#x}", last),
                                format!("{:#x}", base - 1),
                                Size::from_bytes(pad_size).to_string(),
                                Size::from_bytes(pad_size).to_string(),
                            ]);
                        }
                    }
                    let size = talloc.sizes.iter().sum::<u64>();
                    let internal_pad = size - req_size;
                    table.add_row([
                        region_name.to_string(),
                        task_name.to_string(),
                        format!("{:#x}", base),
                        format!("{:#x}", base + size - 1),
                        Size::from_bytes(size).to_string(),
                        Size::from_bytes(internal_pad).to_string(),
                    ]);
                    total += size;
                    loss += internal_pad;

                    last = Some(base + size);
                }
                table.add_row([
                    region_name.to_string(),
                    "(total)".to_string(),
                    String::new(),
                    String::new(),
                    Size::from_bytes(total).to_string(),
                    Size::from_bytes(loss).to_string(),
                ]);
            }
            println!("{table}");
            
            let dir3 = workdir.join("final");
            match std::fs::remove_dir_all(&dir3) {
                Ok(()) => (),
                Err(e) if e.kind() == ErrorKind::NotFound => (),
                e => e.into_diagnostic()?,
            }
            maybe_create_dir(&dir3).into_diagnostic()?;
            let mut built_tasks = vec![];
            let task_link3_text = include_str!("../../../files/task-link3.x");
            buildid.eat(task_link3_text.as_bytes());
            std::fs::write(workdir.join("task-link3.x"), task_link3_text).into_diagnostic()?;
            for taskname in overall_plan.tasks.keys() {
                let built_task = relink_final(
                    &env,
                    &app,
                    &target_spec,
                    &targetroot,
                    &app.tasks[taskname],
                    &initial_build_dir.join(taskname),
                    &dir3.join(taskname),
                    &workdir.join("task-link3.x"),
                    &allocs.tasks[taskname],
                ).with_context(|| format!("failed final link for task {taskname}"))?;

                built_tasks.push(built_task);
            }

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

                for (name, reg) in task.owned_regions {
                    let mem = &app.board.chip.memory[&name].value();

                    config.owned_regions.insert(name, kconfig::MultiRegionConfig {
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

            buildid.hash(&kconfig);

            {
                let linker_script_path = tmpdir.join("memory.x");
                let mut scr = std::fs::File::create(&linker_script_path)
                    .into_diagnostic()?;
                writeln!(scr, "MEMORY {{").into_diagnostic()?;

                writeln!(scr, "STACK (rw): ORIGIN = {:#x}, LENGTH = {:#x}",
                    allocs.kernel.stack.start,
                    allocs.kernel.stack.end - allocs.kernel.stack.start,
                    ).into_diagnostic()?;

                for orig_name in app.board.chip.memory.keys() {
                    let Some(regalloc) = allocs.kernel.by_region.get(&Mem(orig_name.clone())) else {
                        continue;
                    };

                    let name = orig_name.to_ascii_uppercase();
                    let base = regalloc.start;
                    let size = regalloc.end - regalloc.start;

                    writeln!(scr, "{name} (rw): ORIGIN = {base:#x}, LENGTH = {size:#x}").into_diagnostic()?;
                }
                writeln!(scr, "}}").into_diagnostic()?;

                // TODO these values are hardcoded
                writeln!(scr, "_HUBRIS_IMAGE_HEADER_ALIGN = 4;").into_diagnostic()?;
                writeln!(scr, "_HUBRIS_IMAGE_HEADER_SIZE = 0x50;").into_diagnostic()?;
            }
            let mut overall_plan = overall_plan;
            overall_plan.kernel.smuggled_env.insert(
                "HUBRIS_KCONFIG".to_string(),
                ron::ser::to_string(&kconfig).into_diagnostic()?,
            );


            buildid.hash(&overall_plan);

            overall_plan.kernel.smuggled_env.insert(
                "HUBRIS_IMAGE_ID".to_string(),
                buildid.finish().to_string(),
            );

            let kernel_link_text = include_str!("../../../files/kernel-link.x");
            buildid.eat(kernel_link_text.as_bytes());
            std::fs::write(workdir.join("kernel-link.x"), kernel_link_text).into_diagnostic()?;
            do_cargo_build(
                kernel_link_text,
                &overall_plan.kernel,
                LinkStyle::Full,
                &targetroot,
                &tmpdir,
                &dir3,
                "kernel",
                cargo_verbose,
            )?;

            std::fs::remove_file(tmpdir.join("memory.x")).into_diagnostic()?;

            let out = out.unwrap_or_else(|| {
                root.join(format!("{}-build.zip", app.name.value()))
            });
            hubris_build::bundle::make_bundle(
                &app,
                &dir3,
                &out,
            ).into_diagnostic()?;

            banner(format!("Build complete! Archive: {}", out.display()));

            Ok(())
        }
        Cmd::PackHex { bindir, outpath, gdbconfig } => {
            let mut overall_segments = RangeMap::new();
            let mut protohex = vec![];
            let mut start = None;
            let mut all_paths = vec![];

            for dirent in std::fs::read_dir(&bindir).into_diagnostic()? {
                let dirent = dirent.into_diagnostic()?;
                let name = dirent.file_name();
                let name = name.into_string().unwrap();
                let path = dirent.path();
                all_paths.push(path.clone());
                let bytes = std::fs::read(&path).into_diagnostic()?;
                let elf = goblin::elf::Elf::parse(&bytes).into_diagnostic()?;

                for phdr in &elf.program_headers {
                    if phdr.p_type == goblin::elf::program_header::PT_LOAD {
                        let arange = phdr.p_vaddr..phdr.p_vaddr + phdr.p_memsz;
                        if overall_segments.overlaps(&arange) {
                            panic!("range {arange:x?} for {name} overlaps another!");
                        }
                        overall_segments.insert(arange, name.clone());
                        if phdr.p_filesz != 0 {
                            let slice = &bytes[phdr.p_offset as usize..(phdr.p_offset + phdr.p_filesz) as usize];

                            for (i, chunk) in slice.chunks(255).enumerate() {
                                let addr = phdr.p_paddr + i as u64 * 255;
                                protohex.push((addr, chunk.to_vec()));
                            }
                        }
                    }
                }

                if name == "kernel" {
                    start = Some(elf.entry);
                }
            }

            protohex.sort_by_key(|(addr, _)| *addr);

            let mut hex = vec![];
            for (addr, data) in protohex {
                hex.push(ihex::Record::ExtendedLinearAddress((addr >> 16) as u16));
                hex.push(ihex::Record::Data {
                    offset: addr as u16,
                    value: data,
                });
            }
            if let Some(a) = start {
                hex.push(ihex::Record::StartLinearAddress(a as u32));
            }
            hex.push(ihex::Record::EndOfFile);

            let mut rows = vec![];
            for (range, entity) in overall_segments.iter() {
                rows.push((
                    range.start,
                    range.end,
                    entity.clone(),
                ));
            }
            let bottom = overall_segments.first_range_value().unwrap().0.start;
            let top = overall_segments.last_range_value().unwrap().0.end;
            for gap in overall_segments.gaps(&(bottom..top)) {
                rows.push((
                    gap.start,
                    gap.end,
                    "- unused -".to_string(),
                ));
            }
            rows.sort();
            let mut table = comfy_table::Table::new();
            table.load_preset(comfy_table::presets::NOTHING);
            table.set_header(["START", "END (ex)", "OWNER"]);
            table.column_mut(0).unwrap().set_cell_alignment(comfy_table::CellAlignment::Right);
            table.column_mut(1).unwrap().set_cell_alignment(comfy_table::CellAlignment::Right);
            for row in rows {
                table.add_row([
                    format!("{:#x}", row.0),
                    format!("{:#x}", row.1),
                    row.2,
                ]);
            }
            println!("{table}");

            let hexstr = ihex::create_object_file_representation(&hex).into_diagnostic()?;
            std::fs::write(outpath, &hexstr).into_diagnostic()?;

            if let Some(gdbconfig) = gdbconfig {
                let mut g = std::fs::File::create(gdbconfig).into_diagnostic()?;
                for p in &all_paths {
                    if p.ends_with("kernel") {
                        writeln!(g, "file {}", p.display()).into_diagnostic()?;
                    }
                }
                writeln!(g, "target extended-remote localhost:3333").into_diagnostic()?;
                for p in all_paths {
                    if !p.ends_with("kernel") {
                        writeln!(g, "add-symbol-file {}", p.display()).into_diagnostic()?;
                    }
                }
            }

            Ok(())
        }
        Cmd::CheckIdl { path } => {
            let interface = hubris_build::idl::load_interface(path)?;
            println!();
            println!("Got:");
            println!("{interface:#?}");

            let client = hubris_build::idl::codegen::generate_client(&interface)?;
            let client = hubris_build::idl::codegen::format_code(&client);
            println!();
            println!("Client:");
            println!("{client}");
            for (name, t) in &interface.types {
                match t {
                    hubris_build::idl::TypeDef::Enum(e) => {
                        let s = hubris_build::idl::codegen::generate_enum(name, e)?;
                        println!("{}", hubris_build::idl::codegen::format_code(&s));
                    }
                    hubris_build::idl::TypeDef::Struct(_) => (),
                }
            }
            Ok(())
        }
        Cmd::Bundle { cfg_path, bindir, outpath } => {
            // Canonicalize directories and locate/parse input files.
            let root = std::env::var("HUBRIS_PROJECT_ROOT").into_diagnostic()?;
            let root = PathBuf::from(root);
            let doc_src = fs::read_to_string(&cfg_path)
                .into_diagnostic()
                .wrap_err_with(|| format!("can't read {}", cfg_path.display()))?;
            let source = Arc::new(NamedSource::new(
                cfg_path.display().to_string(),
                doc_src.clone(),
            ));
            let doc: kdl::KdlDocument = appcfg::add_source(&source, || {
                Ok(doc_src.parse()?)
            })?;
            let ctx = appcfg::FsContext::from_root(&root);
            let app = appcfg::parse_app(source, &doc, &ctx)?;

            hubris_build::bundle::make_bundle(
                &app,
                &bindir,
                &outpath,
            ).into_diagnostic()?;

            let final_meta = std::fs::metadata(&outpath).into_diagnostic()?;
            println!("built {}: {} on disk", outpath.display(), Size::from_bytes(final_meta.len()));
            Ok(())
        }
    }
}

pub fn guess_intent<'a>(
    value: &str,
    valid_options: impl IntoIterator<Item = &'a String>,
) -> BTreeSet<&'a str> {
    const MAX: usize = 4;

    valid_options.into_iter()
        .filter_map(|option| if strsim::damerau_levenshtein(value, option) <= MAX {
            Some(option.as_str())
        } else {
            None
        })
        .collect()
}

fn maybe_create_dir(path: impl AsRef<Path>) -> std::io::Result<()> {
    match std::fs::create_dir_all(path) {
        Ok(()) => Ok(()),
        Err(e) if e.kind() == ErrorKind::AlreadyExists => Ok(()),
        Err(e) => Err(e),
    }
}
