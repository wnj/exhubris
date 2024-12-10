use std::{collections::{btree_map, BTreeMap, BTreeSet}, fs, io::{ErrorKind, Write as _}, ops::{Range, RangeInclusive}, path::{Path, PathBuf}, process::Command, sync::Arc};

use clap::Parser;
use comfy_table::CellAlignment;
use goblin::elf::program_header::PT_LOAD;
use indexmap::IndexMap;
use miette::{bail, miette, Context, IntoDiagnostic as _, LabeledSpan, NamedSource};
use rangemap::RangeMap;
use size::Size;
use hubris_build::{alloc::{allocate_space, TaskAllocation}, appcfg::{self, AppDef, BuildMethod, BuildPlan}, get_target_spec, BuildEnv, TargetSpec};

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
        Cmd::Build { cfg_path, cargo_verbose } => {
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

            // Interrogate the installed toolchain to discover things like the
            // linker path.
            let env = hubris_build::determine_build_env()?;

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
            for (name, plan) in &overall_plan.tasks {
                do_cargo_build(
                    include_str!("../../../files/task-rlink.x"),
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

            let mut size_reqs: BTreeMap<String, IndexMap<&str, u64>> = BTreeMap::new();
            let temp_link_dir = workdir.join("link2");
            maybe_create_dir(&temp_link_dir).into_diagnostic()?;
            std::fs::write(workdir.join("task-link2.x"), include_str!("../../../files/task-link2.x")).into_diagnostic()?;
            for taskname in overall_plan.tasks.keys() {
                let region_sizes = relink_for_size(
                    &env,
                    &app,
                    &target_spec,
                    &targetroot,
                    taskname,
                    &initial_build_dir.join(taskname),
                    &temp_link_dir.join(taskname),
                    &workdir.join("task-link2.x"),
                )?;

                for (region, range) in region_sizes {
                    let size = range.end - range.start;
                    size_reqs.entry(region).or_default().insert(taskname, size);
                }
            }
            {
                let mut table = comfy_table::Table::new();
                table.load_preset(comfy_table::presets::NOTHING);
                table.set_header(["REGION", "OWNER", "SIZE"]);

                for (taskname, reqs) in &size_reqs {
                    for (memname, size) in reqs {
                        table.add_row([taskname.to_string(), memname.to_string(), Size::from_bytes(*size).to_string()]);
                    }
                }

                println!("{table}");
                println!();
            }

            println!("Allocations:");
            let allocs = allocate_space(&target_spec, &app.board.chip.memory, &size_reqs, &app.kernel)?;

            let mut table = comfy_table::Table::new();
            table.load_preset(comfy_table::presets::NOTHING);
            table.set_header(["MEMORY", "TASK", "START", "END", "SIZE", "LOST"]);
            table.column_mut(2).unwrap().set_cell_alignment(CellAlignment::Right);
            table.column_mut(3).unwrap().set_cell_alignment(CellAlignment::Right);
            table.column_mut(4).unwrap().set_cell_alignment(CellAlignment::Right);
            table.column_mut(5).unwrap().set_cell_alignment(CellAlignment::Right);
            for (region_name, regallocs) in allocs.by_region() {
                let mut regallocs = regallocs.iter().collect::<Vec<_>>();
                regallocs.sort_by_key(|(_name, ta)| *ta.actual.start());
                let mut total = 0;
                let mut loss = 0;

                let mut last = None;
                for (task_name, talloc) in regallocs {
                    let range = &talloc.actual;
                    let req_size = talloc.requested;

                    if let Some(last) = last {
                        if *range.start() != last + 1 {
                            let pad_size = *range.start() - (last + 1);
                            total += pad_size;
                            loss += pad_size;
                            table.add_row([
                                region_name.to_string(),
                                "-pad-".to_string(),
                                format!("{:#x}", last + 1),
                                format!("{:#x}", *range.start() - 1),
                                Size::from_bytes(pad_size).to_string(),
                                Size::from_bytes(pad_size).to_string(),
                            ]);
                        }
                    }
                    let size = range.end() - range.start() + 1;
                    let internal_pad = size - req_size;
                    table.add_row([
                        region_name.to_string(),
                        task_name.to_string(),
                        format!("{:#x}", range.start()),
                        format!("{:#x}", range.end()),
                        Size::from_bytes(size).to_string(),
                        Size::from_bytes(internal_pad).to_string(),
                    ]);
                    total += size;
                    loss += internal_pad;

                    last = Some(*range.end());
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
            std::fs::write(workdir.join("task-link3.x"), include_str!("../../../files/task-link3.x")).into_diagnostic()?;
            for taskname in overall_plan.tasks.keys() {
                let built_task = relink_final(
                    &env,
                    &app,
                    &target_spec,
                    &targetroot,
                    taskname,
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
                    start_at_boot: true,
                };

                for (name, reg) in task.owned_regions {
                    let mem = &app.board.chip.memory[&name].value();

                    let size = (reg.range.end() - reg.range.start()) + 1;
                    config.owned_regions.insert(name, kconfig::MultiRegionConfig {
                        base: u32::try_from(*reg.range.start()).into_diagnostic()?,
                        sizes: vec![u32::try_from(size).into_diagnostic()?],
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

            {
                let linker_script_path = tmpdir.join("memory.x");
                let mut scr = std::fs::File::create(&linker_script_path)
                    .into_diagnostic()?;
                writeln!(scr, "MEMORY {{").into_diagnostic()?;

                writeln!(scr, "STACK (rw): ORIGIN = {:#x}, LENGTH = {:#x}",
                allocs.kernel.stack.start(),
                (allocs.kernel.stack.end() - allocs.kernel.stack.start()) + 1,
                ).into_diagnostic()?;

                for orig_name in app.board.chip.memory.keys() {
                    let Some(regalloc) = allocs.kernel.by_region.get(orig_name) else {
                        continue;
                    };

                    let name = orig_name.to_ascii_uppercase();
                    let base = *regalloc.start();
                    let size = (regalloc.end() - regalloc.start()) + 1;

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
            overall_plan.kernel.smuggled_env.insert(
                "HUBRIS_IMAGE_ID".to_string(),
                "12345678".to_string(),
            );

            std::fs::write(workdir.join("kernel-link.x"), include_str!("../../../files/kernel-link.x")).into_diagnostic()?;
            do_cargo_build(
                include_str!("../../../files/kernel-link.x"),
                &overall_plan.kernel,
                LinkStyle::Full,
                &targetroot,
                &tmpdir,
                &dir3,
                "kernel",
                cargo_verbose,
            )?;

            std::fs::remove_file(tmpdir.join("memory.x")).into_diagnostic()?;

            banner(format!("Build complete! Products in: {}", dir3.display()));

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


            let f = std::fs::File::create(&outpath)
                .into_diagnostic()?;
            let mut z = zip::ZipWriter::new(f);
            let opts = zip::write::SimpleFileOptions::default();

            z.set_comment("hubris build archive v9");

            {
                z.start_file("app.toml", opts).into_diagnostic()?;

                let tasks_toml = app.tasks.iter().map(|(name, task)| {
                    (name.clone(), hubris_build::bundle::TaskToml {
                    })
                }).collect();

                let app_toml = hubris_build::bundle::AppToml {
                    name: app.name.value().clone(),
                    target: app.board.chip.target_triple.value().clone(),
                    board: app.board.name.value().clone(),
                    kernel: hubris_build::bundle::KernelToml {
                        name: match app.kernel.package_source.value() {
                            appcfg::PackageSource::WorkspaceCrate { name } => name.clone(),
                            appcfg::PackageSource::GitCrate { name, .. } => name.clone(),
                        },
                    },
                    tasks: tasks_toml,
                };
                let toml_text = toml::to_string(&app_toml).into_diagnostic()?;
                z.write_all(toml_text.as_bytes()).into_diagnostic()?;
            }

            let mut collected_segments = BTreeMap::new();

            let mut little_endian = BTreeSet::new();
            let mut is_64 = BTreeSet::new();
            let mut elf_machine = BTreeSet::new();
            let mut elf_os_abi = BTreeSet::new();
            let mut elf_abi_version = BTreeSet::new();
            let mut kernel_entry = None;

            for dirent in std::fs::read_dir(&bindir).into_diagnostic()? {
                let dirent = dirent.into_diagnostic()?;
                if dirent.path().is_file() {
                    let name = dirent.file_name();
                    let name = name.to_str().unwrap();

                    let elf_bytes = std::fs::read(dirent.path())
                        .into_diagnostic()?;
                    z.start_file(format!("elf/{name}"), opts)
                        .into_diagnostic()?;
                    z.write_all(&elf_bytes).into_diagnostic()?;

                    let elf = goblin::elf::Elf::parse(&elf_bytes)
                        .into_diagnostic()?;
                    little_endian.insert(elf.little_endian);
                    is_64.insert(elf.is_64);
                    elf_machine.insert(elf.header.e_machine);
                    elf_os_abi.insert(elf.header.e_ident[goblin::elf::header::EI_OSABI]);
                    elf_abi_version.insert(elf.header.e_ident[goblin::elf::header::EI_ABIVERSION]);

                    for phdr in &elf.program_headers {
                        if phdr.p_type != goblin::elf::program_header::PT_LOAD {
                            continue;
                        }
                        if phdr.p_filesz == 0 {
                            continue;
                        }
                        let start = usize::try_from(phdr.p_offset).unwrap();
                        let end = start + usize::try_from(phdr.p_filesz).unwrap();
                        let segment_bytes = elf_bytes[start..end].to_vec();
                        collected_segments.insert(
                            phdr.p_paddr,
                            segment_bytes,
                        );
                    }

                    if name == "kernel" {
                        kernel_entry = Some(elf.entry);
                    }
                }
            }

            let base_addr = *collected_segments.first_key_value().unwrap().0;
            let (_final_addr, flattened) = collected_segments.into_iter()
                .fold((None, vec![]), |(last_addr, mut flattened), (addr, bytes)| {
                    println!("{addr:#x}");
                    if let Some(la) = last_addr {
                        let gap_size = dbg!(addr) - dbg!(la);
                        let new_len = flattened.len() + usize::try_from(gap_size).unwrap();
                        flattened.resize(new_len, 0xFF);
                    }
                    let n = bytes.len();
                    flattened.extend(bytes);

                    (Some(addr + u64::try_from(n).unwrap()), flattened)
                });

            if little_endian.len() == 2 {
                bail!("mix of little- and big-endian objects?");
            }
            let little_endian = little_endian.pop_last().unwrap();
            if is_64.len() == 2 {
                bail!("mix of 32- and 64-bit objects?");
            }
            let is_64 = is_64.pop_last().unwrap();
            if elf_os_abi.len() == 2 {
                bail!("mix of OS ABI?");
            }
            let elf_os_abi = elf_os_abi.pop_last().unwrap();
            if elf_abi_version.len() == 2 {
                bail!("mix of ABI versions?");
            }
            let elf_abi_version = elf_abi_version.pop_last().unwrap();
            if elf_machine.len() == 2 {
                bail!("mix of machine types?");
            }
            let elf_machine = elf_machine.pop_last().unwrap();

            let mut final_buf = vec![];
            let mut w = object::write::elf::Writer::new(
                if little_endian {
                    object::Endianness::Little
                } else {
                    object::Endianness::Big
                },
                is_64,
                &mut final_buf,
            );
            w.reserve_file_header();
            w.reserve_program_headers(1);
            let offset = w.reserve(flattened.len(), 8);
            let _index = w.reserve_section_index();
            let name = w.add_section_name(b".sec1");
            w.reserve_shstrtab_section_index();
            w.reserve_shstrtab();
            w.reserve_section_headers();

            w.write_file_header(&object::write::elf::FileHeader {
                os_abi: elf_os_abi,
                abi_version: elf_abi_version,
                e_type: object::elf::ET_REL,
                e_machine: elf_machine,
                e_entry: kernel_entry.unwrap(),
                e_flags: 0,
            }).into_diagnostic()?;
            w.write_align_program_headers();
            let len64 = u64::try_from(flattened.len()).unwrap();
            w.write_program_header(&object::write::elf::ProgramHeader {
                p_type: object::elf::PT_LOAD,
                p_flags: object::elf::PF_R,
                p_offset: u64::try_from(offset).unwrap(),
                p_vaddr: base_addr,
                p_paddr: base_addr,
                p_filesz: len64,
                p_memsz: len64,
                p_align: 0,
            });

            w.write_align(8);
            assert_eq!(w.len(), offset);
            w.write(&flattened);
            w.write_shstrtab();
            w.write_null_section_header();

            w.write_section_header(&object::write::elf::SectionHeader {
                name: Some(name),
                sh_type: object::elf::SHT_PROGBITS,
                sh_flags: (object::elf::SHF_WRITE | object::elf::SHF_ALLOC) as u64,
                sh_addr: base_addr,
                sh_offset: u64::try_from(offset).unwrap(),
                sh_size: len64,
                sh_link: 0,
                sh_info: 0,
                sh_addralign: 1,
                sh_entsize: 0,
            });

            w.write_shstrtab_section_header();

            assert_eq!(w.reserved_len(), w.len());

            z.start_file("img/final.elf", opts)
                .into_diagnostic()?;
            z.write_all(&final_buf).into_diagnostic()?;

            drop(z);
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

struct BuiltTask {
    name: String,
    entry: OwnedAddress,
    initial_stack_pointer: OwnedAddress,
    owned_regions: BTreeMap<String, OwnedRegion>,
}

struct OwnedRegion {
    range: RangeInclusive<u64>,
}

struct OwnedAddress {
    region: String,
    offset: u64,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum LinkStyle {
    Partial,
    Full,
}

#[allow(clippy::too_many_arguments)]
fn do_cargo_build(
    linker_script_text: &str,
    plan: &BuildPlan,
    link_style: LinkStyle,
    targetroot: &Path,
    tmpdir: &Path,
    outdir: &Path,
    product_name: &str,
    cargo_verbose: bool,
) -> miette::Result<()> {
    let comma_features = itertools::join(&plan.cargo_features, ",");
    let mut rows = vec![
        ("Product", product_name),
        ("Package", &plan.package_name),
        ("Binary", &plan.bin_name),
        ("Target", &plan.target_triple),
        ("Default Features", if plan.default_features { "true" } else { "false" }),
        ("Features", &comma_features),
    ];
    if let Some(tc) = &plan.toolchain_override {
        rows.push(("Toolchain Override", tc));
    }
    if !plan.rustflags.is_empty() {
        rows.push(("RUSTFLAGS", &plan.rustflags));
    }
    for (k, v) in &plan.smuggled_env {
        rows.push((k, v));
    }
    simple_table(rows);
    
    let mut cmd = Command::new("cargo");

    if let Some(tc) = &plan.toolchain_override {
        cmd.arg(format!("+{tc}"));
    }

    let linker_script_copy = tmpdir.join("link.x");
    std::fs::write(&linker_script_copy, linker_script_text).into_diagnostic()?;

    let mut rustflags = format!("-C link-arg=-L{} -C link-arg=-T{}{}",
            tmpdir.display(),
            linker_script_copy.display(),
            match link_style {
                LinkStyle::Partial => " -C link-arg=-r",
                LinkStyle::Full => "",
            },
    );
    if !plan.rustflags.is_empty() {
        rustflags.push(' ');
        rustflags.push_str(&plan.rustflags);
    }

    cmd.env("RUSTFLAGS", rustflags);

    let product_path = match &plan.method {
        BuildMethod::CargoWorkspaceBuild => {
            cmd.args(["build", "--release"]);
            cmd.args(["-p", &plan.package_name, "--bin", &plan.bin_name]);

            let mut outloc = targetroot.join(&plan.target_triple);
            outloc.push("release");
            outloc.push(&plan.bin_name);
            outloc
        }
        BuildMethod::CargoInstallGit { repo, rev } => {
            cmd.args(["install", "--locked", "--no-track", "--force"]);
            cmd.arg(&plan.package_name);
            cmd.args(["--bin", &plan.bin_name]);
            cmd.args(["--git", repo]);
            cmd.args(["--rev", rev]);

            let installroot = tmpdir.join(format!("{product_name}.cargo-install"));
            let targetdir = tmpdir.join(format!("{product_name}.cargo-target"));

            cmd.arg("--root");
            cmd.arg(&installroot);

            let bindir = installroot.join("bin");
            let binpath = bindir.join(&plan.bin_name);

            // Extend the PATH to add our output directory so that cargo won't
            // unconditionally warn about every build, sigh.
            match std::env::var_os("PATH") {
                Some(path) => {
                    let mut parts = std::env::split_paths(&path).collect::<Vec<_>>();
                    parts.push(bindir.clone());
                    let new_path = std::env::join_paths(parts).into_diagnostic()?;
                    cmd.env("PATH", new_path);
                }
                None => {
                    cmd.env("PATH", &bindir);
                }
            }

            cmd.env("CARGO_TARGET_DIR", targetdir);

            binpath
        }
    };

    if cargo_verbose {
        cmd.arg("--verbose");
    }

    cmd.args(["--target", &plan.target_triple]);

    if !plan.default_features {
        cmd.arg("--no-default-features");
    }
    if !plan.cargo_features.is_empty() {
        cmd.args(["--features", &comma_features]);
    }

    for (k, v) in &plan.smuggled_env {
        cmd.env(k, v);
    }

    let status = cmd.status().into_diagnostic()?;
    if !status.success() {
        bail!("failed to build, see output");
    }

    std::fs::remove_file(linker_script_copy).into_diagnostic()?;

    let final_path = outdir.join(product_name);
    std::fs::copy(&product_path, &final_path).into_diagnostic()
        .with_context(|| format!("copying {} to {}", product_path.display(), final_path.display()))?;

    Ok(())
}

fn banner(content: impl core::fmt::Display) {
    let mut table = comfy_table::Table::new();
    table.load_preset(comfy_table::presets::UTF8_FULL);
    table.apply_modifier(comfy_table::modifiers::UTF8_ROUND_CORNERS);
    table.set_content_arrangement(comfy_table::ContentArrangement::Dynamic);

    table.add_row([content]);

    println!();
    println!();
    println!("{table}");
    println!();
}

fn simple_table<'a>(content: impl IntoIterator<Item = (&'a str, &'a str)>) {
    let mut table = comfy_table::Table::new();
    table.load_preset(comfy_table::presets::UTF8_FULL);
    table.apply_modifier(comfy_table::modifiers::UTF8_ROUND_CORNERS);
    table.apply_modifier(comfy_table::modifiers::UTF8_SOLID_INNER_BORDERS);
    table.set_content_arrangement(comfy_table::ContentArrangement::Dynamic);

    for row in content {
        table.add_row([row.0, row.1]);
    }

    println!();
    println!();
    println!("{table}");
    println!();
}

#[allow(clippy::too_many_arguments)]
fn relink(
    env: &BuildEnv,
    app: &AppDef,
    target: &TargetSpec,
    targetroot: &Path,
    taskname: &str,
    inpath: &Path,
    outpath: &Path,
    linker_script: &Path,
    alloc: &BTreeMap<String, TaskAllocation>,
) -> miette::Result<(BTreeMap<String, OwnedRegion>, Option<OwnedAddress>)> {
    let memory_frag_path = targetroot.join("memory.x");
    let mut scr = std::fs::File::create(&memory_frag_path)
        .into_diagnostic()?;
    writeln!(scr, "MEMORY {{").into_diagnostic()?;
    let def = &app.tasks[taskname];
    let mut initial_stack_pointer = None;
    let mut owned_regions = BTreeMap::new();
    for orig_name in app.board.chip.memory.keys() {
        let Some(regalloc) = alloc.get(orig_name) else {
            continue;
        };

        owned_regions.insert(orig_name.to_string(), OwnedRegion {
            range: regalloc.actual.clone(),
        });

        let name = orig_name.to_ascii_uppercase();
        let mut base = *regalloc.actual.start();
        let mut size = (regalloc.actual.end() - regalloc.actual.start()) + 1;

        if name == "RAM" {
            // deduct stack

            writeln!(scr, "STACK (rw): ORIGIN = {base:#x}, LENGTH = {:#x}", def.stack_size.value()).into_diagnostic()?;
            base += *def.stack_size.value();
            size -= *def.stack_size.value();
            initial_stack_pointer = Some(OwnedAddress {
                region: orig_name.to_string(),
                offset: *def.stack_size.value(),
            });
        }
        writeln!(scr, "{name} (rw): ORIGIN = {base:#x}, LENGTH = {size:#x}").into_diagnostic()?;
    }
    writeln!(scr, "}}").into_diagnostic()?;
    writeln!(scr, "SECTIONS {{").into_diagnostic()?;
    for (section, memory) in &app.tasks[taskname].sections {
        let leader = if section.starts_with('.') { "" } else { "." };
        let memory = memory.to_ascii_uppercase();
        writeln!(scr, "{leader}{section} (NOLOAD) : ALIGN(4) {{").into_diagnostic()?;
        writeln!(scr, "  *({leader}{section} {leader}{section}.*);").into_diagnostic()?;
        writeln!(scr, "}} > {memory}").into_diagnostic()?;
    }
    writeln!(scr, "  ").into_diagnostic()?;
    writeln!(scr, "}} INSERT AFTER .uninit").into_diagnostic()?;
    drop(scr);

    let mut ldcmd = Command::new(&env.linker_path);
    ldcmd.arg(inpath);
    ldcmd.arg("-o").arg(outpath);
    ldcmd.arg(format!("-T{}", linker_script.display()));
    ldcmd.arg("--gc-sections");
    ldcmd.args(["-m", &target.bfd_name]);

    // TODO: are these still necessary?
    ldcmd.args(["-z", "common-page-size=0x20"]);
    ldcmd.args(["-z", "max-page-size=0x20"]);

    ldcmd.current_dir(targetroot);

    let ldstatus = ldcmd.status().into_diagnostic()?;
    if !ldstatus.success() {
        return Err(miette!("command failed"));
    }

    std::fs::remove_file(memory_frag_path).into_diagnostic()?;
    Ok((owned_regions, initial_stack_pointer))
}

#[allow(clippy::too_many_arguments)]
fn relink_for_size(
    env: &BuildEnv,
    app: &AppDef,
    target: &TargetSpec,
    targetroot: &Path,
    taskname: &str,
    inpath: &Path,
    outpath: &Path,
    linker_script: &Path,
) -> miette::Result<BTreeMap<String, Range<u64>>> {
    let alloc_everything = app.board.chip.memory.iter()
        .map(|(name, regdef)| (name.clone(), TaskAllocation {
            requested: 0,
            actual: *regdef.value().base.value()..=regdef.value().base.value() + regdef.value().size.value() - 1,
        }))
    .collect();
    relink(env, app, target, targetroot, taskname, inpath, outpath, linker_script, &alloc_everything)?;

    let file_image = std::fs::read(outpath).into_diagnostic()?;
    let elf = goblin::elf::Elf::parse(&file_image).into_diagnostic()?;

    let mut region_sizes = app.board.chip.memory.iter()
        .map(|(name, region)| (name.clone(), *region.value().base.value()..*region.value().base.value()))
        .collect::<BTreeMap<_, _>>();
    for phdr in &elf.program_headers {
        if phdr.p_type != PT_LOAD {
            continue;
        }
        let (regname, _reg) = app.board.chip.memory.iter()
            .find(|(_name, reg)| *reg.value().base.value() <= phdr.p_vaddr && (phdr.p_vaddr + phdr.p_memsz) <= (reg.value().base.value() + reg.value().size.value()))
            .expect("internal inconsistency");

        let sz = region_sizes.get_mut(regname).expect("internal inconsistency");
        sz.start = u64::min(sz.start, phdr.p_vaddr);
        sz.end = u64::max(sz.end, phdr.p_vaddr + phdr.p_memsz);

        if phdr.p_vaddr != phdr.p_paddr {
            let (regname, _reg) = app.board.chip.memory.iter()
                .find(|(_name, reg)| *reg.value().base.value() <= phdr.p_paddr && (phdr.p_paddr + phdr.p_filesz) <= (reg.value().base.value() + reg.value().size.value()))
                .expect("internal inconsistency");

            let sz = region_sizes.get_mut(regname).expect("internal inconsistency");
            sz.start = u64::min(sz.start, phdr.p_paddr);
            sz.end = u64::max(sz.end, phdr.p_paddr + phdr.p_filesz);
        }
    }
    Ok(region_sizes)
}

#[allow(clippy::too_many_arguments)]
fn relink_final(
    env: &BuildEnv,
    app: &AppDef,
    target: &TargetSpec,
    targetroot: &Path,
    taskname: &str,
    inpath: &Path,
    outpath: &Path,
    linker_script: &Path,
    alloc: &BTreeMap<String, TaskAllocation>,
) -> miette::Result<BuiltTask> {
    let (owned_regions, initial_stack_pointer) = relink(env, app, target, targetroot, taskname, inpath, outpath, linker_script, alloc)?;

    let file_image = std::fs::read(outpath).into_diagnostic()?;
    let elf = goblin::elf::Elf::parse(&file_image).into_diagnostic()?;

    let entry_region = owned_regions.iter()
    .find(|(_name, reg)| elf.entry >= *reg.range.start()
        && elf.entry <= *reg.range.end());
    let entry_region = entry_region.expect("invalid entry point");

    let entry = OwnedAddress {
        region: entry_region.0.to_string(),
        offset: elf.entry - entry_region.1.range.start(),
    };
    let initial_stack_pointer = initial_stack_pointer.expect("missing RAM region?");

    Ok(BuiltTask {
        name: taskname.to_string(),
        entry,
        initial_stack_pointer,
        owned_regions,
    })
}

