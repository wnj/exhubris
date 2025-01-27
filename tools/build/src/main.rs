use std::{collections::{btree_map, BTreeMap, BTreeSet}, ffi::OsStr, fs, io::{ErrorKind, Write as _}, ops::Range, path::{Path, PathBuf}, process::Command, sync::Arc, time::Instant};

use clap::Parser;
use comfy_table::CellAlignment;
use goblin::elf::program_header::PT_LOAD;
use miette::{bail, miette, Context, IntoDiagnostic as _, LabeledSpan, NamedSource};
use rangemap::RangeMap;
use size::Size;
use hubris_build::{alloc::{allocate_space, TaskAllocation}, appcfg::{self, AppDef, BuildMethod, BuildPlan, TaskDef}, buildid::BuildId, get_target_spec, BuildEnv, TargetSpec};
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

struct BuiltTask {
    name: String,
    entry: OwnedAddress,
    initial_stack_pointer: OwnedAddress,
    owned_regions: BTreeMap<String, OwnedRegion>,
}

struct OwnedRegion {
    base: u64,
    sizes: Vec<u64>,
}

impl OwnedRegion {
    pub fn contains(&self, addr: u64) -> bool {
        let size = self.sizes.iter().sum::<u64>();
        addr >= self.base && addr < (self.base + size)
    }
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

/// Creates a `Command` to run `command`, but with environment conflicts
/// removed.
///
/// This will prevent the resulting `Command` from inheriting any environment
/// variables starting with `HUBRIS_`, except for those explicitly set after
/// this returns.
///
/// This is intended to help avoid confusing state leaks from the user's shell
/// environment into builds.
fn cmd_with_clean_env(command: impl AsRef<OsStr>) -> Command {
    let mut cmd = Command::new(command);

    for (name, _value) in std::env::vars() {
        if name.starts_with("HUBRIS_") {
            cmd.env_remove(name);
        }
    }

    cmd
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
    let joined_rustflags = plan.rustflags.join("\n");
    rows.push(("RUSTFLAGS", &joined_rustflags));
    for (k, v) in &plan.smuggled_env {
        rows.push((k, v));
    }
    simple_table(rows);
    
    let mut cmd = cmd_with_clean_env("cargo");

    if let Some(tc) = &plan.toolchain_override {
        cmd.arg(format!("+{tc}"));
    }

    let linker_script_copy = tmpdir.join("link.x");
    std::fs::write(&linker_script_copy, linker_script_text).into_diagnostic()?;

    let mut rustflags = vec![
        format!("-Clink-arg=-L{}", tmpdir.display()),
        format!("-Clink-arg=-T{}", linker_script_copy.display()),
    ];
    match link_style {
        LinkStyle::Partial => rustflags.push("-Clink-arg=-r".to_string()),
        LinkStyle::Full => (),
    }
    rustflags.extend(plan.rustflags.iter().cloned());

    cmd.env("CARGO_ENCODED_RUSTFLAGS", rustflags.join("\u{001f}"));

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

/// Performs a second link pass on a partially linked executable.
///
/// `env`, `app`, `target`, and `targetroot` describe the overall build
/// configuration.
///
/// `def` specifies the task in the `app`. This should be a reference to one of
/// the `app.tasks`.
///
/// `inpath` gives the location of the partially linked executable, while
/// `outpath` will be written with the result.
///
/// `linker_script` gives the location of the linker script to use in this pass.
/// The script should contain a directive to include `memory.x`.
///
/// `alloc` is the results of memory allocation for the task, after the initial
/// build and analysis phase. It is a map from memory name (as given in the
/// chipdef) to allocation requirements.
///
/// On success, returns:
/// - A map from memory name to regions owned by this task.
/// - The task's initial stack pointer, in terms of those regions.
#[allow(clippy::too_many_arguments)]
fn relink(
    env: &BuildEnv,
    app: &AppDef,
    target: &TargetSpec,
    targetroot: &Path,
    def: &TaskDef,
    inpath: &Path,
    outpath: &Path,
    linker_script: &Path,
    alloc: &BTreeMap<String, TaskAllocation>,
) -> miette::Result<(BTreeMap<String, OwnedRegion>, Option<OwnedAddress>)> {
    // Generate the linker script "memory fragment" in the targetroot. This file
    // has to be called "memory.x" because it's pulled in by an include
    // directive under that name by the reusable part of the linker script.
    //
    // This fragment consists of
    // - The MEMORY section
    // - A SECTIONS add-on stanza containing any additional section mappings
    //   required for the task.
    let memory_frag_path = targetroot.join("memory.x");
    let mut scr = std::fs::File::create(&memory_frag_path)
        .into_diagnostic()?;
    writeln!(scr, "MEMORY {{").into_diagnostic()?;

    // During this same pass over the task memory map, we're going to determine
    // the value of the initial stack pointer and the set of owned memory
    // regions.
    let mut initial_stack_pointer = None;
    let mut owned_regions = BTreeMap::new();

    // For each memory type defined in the chipdef, check if the task has
    // any requirements in that memory.
    for orig_name in app.board.chip.memory.keys() {
        let Some(regalloc) = alloc.get(orig_name) else {
            // Nope!
            continue;
        };

        owned_regions.insert(orig_name.to_string(), OwnedRegion {
            base: regalloc.base,
            sizes: regalloc.sizes.clone(),
        });

        // Linker memory names are conventionally UPPERCASE, so:
        let name = orig_name.to_ascii_uppercase();

        let mut base = regalloc.base;
        let mut size = regalloc.calculate_size();

        if name == "RAM" {
            // We recognize this magical memory name as the place where stacks
            // go. Stacks are inserted at the bottom of the task's RAM area.
            //
            // TODO: the fact that this uses the memory name is kind of grody.

            // We generate a "memory" named STACK to describe this.
            writeln!(scr, "STACK (rw): ORIGIN = {base:#x}, LENGTH = {:#x}", def.stack_size.value()).into_diagnostic()?;
            // Deduct the stack from the overall RAM size.
            base += *def.stack_size.value();
            size -= *def.stack_size.value();
            // The stack pointer will start at the top of that deduction.
            initial_stack_pointer = Some(OwnedAddress {
                region: orig_name.to_string(),
                offset: *def.stack_size.value(),
            });
        }
        writeln!(scr, "{name} (rw): ORIGIN = {base:#x}, LENGTH = {size:#x}").into_diagnostic()?;
    }
    writeln!(scr, "}}").into_diagnostic()?;

    // And now, the SECTIONS section, to insert sections with your sections so
    // you can section your sections. ...
    //
    // Anyway, this handles any explicitly declared additional memory sections.
    writeln!(scr, "SECTIONS {{").into_diagnostic()?;
    for (section, memory) in &def.sections {
        // We tolerate section names not starting with a dot in the config, but
        // for consistency, add a dot in the linker script.
        let leader = if section.starts_with('.') { "" } else { "." };

        let memory = memory.to_ascii_uppercase();
        writeln!(scr, "{leader}{section} (NOLOAD) : ALIGN(4) {{").into_diagnostic()?;
        writeln!(scr, "  *({leader}{section} {leader}{section}.*);").into_diagnostic()?;
        writeln!(scr, "}} > {memory}").into_diagnostic()?;
    }
    writeln!(scr, "  ").into_diagnostic()?;
    // .uninit is our final useful section in the linker script, and generally
    // where things like this want to go.
    writeln!(scr, "}} INSERT AFTER .uninit").into_diagnostic()?;

    // All done!
    drop(scr);

    // Run the linker on the provided linker script, plus our fragment.
    let mut ldcmd = cmd_with_clean_env(&env.linker_path);
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

    // Delete the memory fragment file, to avoid surprises.
    std::fs::remove_file(memory_frag_path).into_diagnostic()?;

    Ok((owned_regions, initial_stack_pointer))
}

/// Re-links a partially linked executable against a bogus memory map, to
/// determine its resource requirements.
///
/// Specifically, this links a partially-linked task as though it had sole
/// control of all of the memory in the chipdef. We then analyze the resulting
/// ELF executable to determine memory requirements in each memory region, and
/// return that.
///
/// `env`, `app`, `target`, and `targetroot` define the build environment.
///
/// `def` defines the task, and should be a reference to one of `app.tasks`.
///
/// `inpath` gives the path to the partially-linked ELF executable, and
/// `outpath` gives a location where we can write a linked-but-bogus equivalent.
///
/// `linker_script` is a path to the linker script to use at this stage.
#[allow(clippy::too_many_arguments)]
fn relink_for_size(
    env: &BuildEnv,
    app: &AppDef,
    target: &TargetSpec,
    targetroot: &Path,
    def: &TaskDef,
    inpath: &Path,
    outpath: &Path,
    linker_script: &Path,
) -> miette::Result<BTreeMap<String, Range<u64>>> {
    // Make an "allocation result" that assigns this task all of memory.
    let alloc_everything = app.board.chip.memory.iter()
        .map(|(name, regdef)| (name.clone(), TaskAllocation {
            requested: 0,
            base: *regdef.value().base.value(),
            sizes: vec![*regdef.value().size.value()],
        }))
    .collect();
    // Relink the task to that allocation.
    relink(env, app, target, targetroot, def, inpath, outpath, linker_script, &alloc_everything)?;

    // Load the result and analyze the memory layout.
    let file_image = std::fs::read(outpath).into_diagnostic()?;
    let elf = goblin::elf::Elf::parse(&file_image).into_diagnostic()?;

    // Initialize the size map with an entry for each type of memory defined in
    // the chipdef, but with a zero-size range at its base. We will enlarge
    // these ranges as required.
    let mut region_sizes = app.board.chip.memory.iter()
        .map(|(name, region)| (name.clone(), *region.value().base.value()..*region.value().base.value()))
        .collect::<BTreeMap<_, _>>();

    for phdr in &elf.program_headers {
        // Skip metadata and whatnot.
        if phdr.p_type != PT_LOAD {
            continue;
        }
        // Figure out which region contains this phdr.
        let (regname, _reg) = app.board.chip.memory.iter()
            .find(|(_name, reg)| *reg.value().base.value() <= phdr.p_vaddr && (phdr.p_vaddr + phdr.p_memsz) <= (reg.value().base.value() + reg.value().size.value()))
            .expect("internal inconsistency");

        // Borrow its size range in our output table.
        let sz = region_sizes.get_mut(regname).expect("internal inconsistency");
        // Enlarge the range to contain all of this item.
        sz.start = u64::min(sz.start, phdr.p_vaddr);
        sz.end = u64::max(sz.end, phdr.p_vaddr + phdr.p_memsz);

        if phdr.p_vaddr != phdr.p_paddr {
            // This is an "initialized data" phdr, like the .data section. This
            // means it appears twice, once in RAM at its full memsz, and once
            // somewhere else (generally flash) with a possibly smaller filsz.
            //
            // Repeat the process, but use the paddr and filsz.
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

/// Relink a partially-linked task executable in its final location.
///
/// `env`, `app`, `target`, and `targetroot` define the build environment.
///
/// `def` defines the task, and should be a reference to one of `app.tasks`.
///
/// `inpath` gives the path to the partially-linked ELF executable, and
/// `outpath` is where the final linked result will be placed.
///
/// `linker_script` is the path to the linker script we'll use for the final
/// link.
///
/// `alloc` is the set of allocation decisions for this task, defining its final
/// placement.
/// 
/// On success, returns a `BuiltTask` summarizing the output.
#[allow(clippy::too_many_arguments)]
fn relink_final(
    env: &BuildEnv,
    app: &AppDef,
    target: &TargetSpec,
    targetroot: &Path,
    def: &TaskDef,
    inpath: &Path,
    outpath: &Path,
    linker_script: &Path,
    alloc: &BTreeMap<String, TaskAllocation>,
) -> miette::Result<BuiltTask> {
    // Use the common relink routine to do most of the work.
    let (owned_regions, initial_stack_pointer) = relink(env, app, target, targetroot, def, inpath, outpath, linker_script, alloc)?;

    // There's one specific piece of information that we don't have yet: the
    // binary's entry address. While most Hubris binaries build themselves to
    // have their entry point at the base of flash, we don't actually _require_
    // this.
    //
    // Instead, we're going to parse the ELF headers.
    let file_image = std::fs::read(outpath).into_diagnostic()?;
    let elf = goblin::elf::Elf::parse(&file_image).into_diagnostic()?;

    let entry_region = owned_regions.iter()
        .find(|(_name, reg)| reg.contains(elf.entry));
    let entry_region = entry_region.expect("invalid entry point");

    let entry = OwnedAddress {
        region: entry_region.0.to_string(),
        offset: elf.entry - entry_region.1.base,
    };
    let initial_stack_pointer = initial_stack_pointer.expect("missing RAM region?");

    Ok(BuiltTask {
        name: def.name.clone(),
        entry,
        initial_stack_pointer,
        owned_regions,
    })
}
