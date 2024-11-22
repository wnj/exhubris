use std::{collections::{BTreeMap, BTreeSet}, fs, io::{ErrorKind, Write as _}, ops::{Range, RangeInclusive}, path::{Path, PathBuf}, process::Command, sync::Arc};

use clap::Parser;
use comfy_table::CellAlignment;
use goblin::elf::program_header::PT_LOAD;
use indexmap::IndexMap;
use miette::{bail, miette, Context, IntoDiagnostic as _, LabeledSpan, NamedSource};
use rangemap::RangeMap;
use size::Size;
use tools::{appcfg::{AppDef, BuildMethod, BuildPlan, KernelDef, RegionDef}, BuildEnv};

use hubris_build_kconfig as kconfig;

#[derive(Parser)]
struct Tool {
    #[clap(subcommand)]
    cmd: Cmd,
}

#[derive(Parser)]
enum Cmd {
    Build {
        #[clap(short, long)]
        root: PathBuf,
        cfg_path: PathBuf,

        #[clap(long)]
        cargo_verbose: bool,
    },
    Pack {
        bindir: PathBuf,
        outpath: PathBuf,

        #[clap(short, long)]
        gdbconfig: Option<PathBuf>,
    },
}

fn main() -> miette::Result<()> {
    let args = Tool::parse();

    match args.cmd {
        Cmd::Build { cfg_path, root, cargo_verbose } => {
            // Canonicalize directories and locate/parse input files.
            let root = root.canonicalize().into_diagnostic()?;
            let doc_src = fs::read_to_string(&cfg_path)
                .into_diagnostic()
                .wrap_err_with(|| format!("can't read {}", cfg_path.display()))?;
            let source = Arc::new(NamedSource::new(
                cfg_path.display().to_string(),
                doc_src.clone(),
            ));
            let doc: kdl::KdlDocument = tools::appcfg::add_source(&source, || {
                Ok(doc_src.parse()?)
            })?;
            let ctx = tools::appcfg::FsContext::from_root(&root);
            let app = tools::appcfg::parse_app(source, &doc, &ctx)?;

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
            let env = tools::determine_build_env()?;

            // Print the kickoff header:
            simple_table([
                ("App name", app.name.value().as_str()),
                ("Config path", &cfg_path.display().to_string()),
                ("Workspace root", &root.display().to_string()),
                ("Host platform", &env.host_triple),
                ("Default toolchain", &env.release),
            ]);

            // Analyze the app and make a build plan.
            let overall_plan = tools::appcfg::plan_build(&app)?;
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
                    &root.join("task-rlink.x"),
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

            std::fs::copy(root.join("task-link2.x"), targetroot.join("task-link2.x")).into_diagnostic()?;
            let mut size_reqs: BTreeMap<String, IndexMap<&str, u64>> = BTreeMap::new();
            let temp_link_dir = workdir.join("link2");
            maybe_create_dir(&temp_link_dir).into_diagnostic()?;
            for taskname in overall_plan.tasks.keys() {
                let region_sizes = relink_for_size(
                    &env,
                    &app,
                    &target_spec,
                    &targetroot,
                    taskname,
                    &initial_build_dir.join(taskname),
                    &temp_link_dir.join(taskname),
                    &root.join("task-link2.x"),
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

                let mut last = None;
                for (task_name, talloc) in regallocs {
                    let range = &talloc.actual;
                    let req_size = talloc.requested;

                    if let Some(last) = last {
                        if *range.start() != last + 1 {
                            dbg!(range.start());
                            dbg!(last);
                            let pad_size = *range.start() - last;
                            table.add_row([
                                region_name.to_string(),
                                "-pad-".to_string(),
                                format!("{last:#x}"),
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

                    last = Some(*range.end());
                }
            }
            println!("{table}");
            
            std::fs::copy(root.join("task-link3.x"), targetroot.join("task-link3.x")).into_diagnostic().context("copying link3")?;
            let dir3 = workdir.join("final");
            match std::fs::remove_dir_all(&dir3) {
                Ok(()) => (),
                Err(e) if e.kind() == ErrorKind::NotFound => (),
                e => e.into_diagnostic()?,
            }
            maybe_create_dir(&dir3).into_diagnostic()?;
            let mut built_tasks = vec![];
            for taskname in overall_plan.tasks.keys() {
                let built_task = relink_final(
                    &env,
                    &app,
                    &target_spec,
                    &targetroot,
                    taskname,
                    &initial_build_dir.join(taskname),
                    &dir3.join(taskname),
                    &root.join("task-link3.x"),
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
                    let size = (reg.range.end() - reg.range.start()) + 1;
                    config.owned_regions.insert(name, kconfig::MultiRegionConfig {
                        base: u32::try_from(*reg.range.start()).into_diagnostic()?,
                        sizes: vec![u32::try_from(size).into_diagnostic()?],
                        attributes: kconfig::RegionAttributes {
                            read: true, // TODO
                            write: true, // TODO
                            execute: true, // TODO
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

            do_cargo_build(
                &root.join("kernel-link.x"),
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
        Cmd::Pack { bindir, outpath, gdbconfig } => {
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

#[derive(Copy, Clone, Debug)]
enum SizeRule {
    /// Allocations must be a power-of-two in size.
    PowerOfTwo,
    /// Allocations must be an integer multiple of this number.
    MultipleOf(u64),
}

struct TargetSpec {
    /// Rule determining legal allocation sizes on this target.
    size_rule: SizeRule,
    /// Minimum allocation size on this target.
    alloc_minimum: u64,
    /// Stack alignment requirement in bytes.
    stack_align: u64,
    /// BFD architecture name.
    bfd_name: String,
}

impl TargetSpec {
    fn align_before_allocation(&self, addr: u64) -> u64 {
        addr.next_multiple_of(self.alloc_minimum)
    }

    fn align_for_allocation_size(&self, addr: u64, size: u64) -> u64 {
        let size = u64::max(size, self.alloc_minimum);
        match self.size_rule {
            SizeRule::PowerOfTwo => {
                addr.next_multiple_of(size)
            }
            SizeRule::MultipleOf(n) => {
                addr.next_multiple_of(n)
            }
        }
    }

    fn round_allocation_size(&self, size: u64) -> u64 {
        let size = u64::max(size, self.alloc_minimum);
        match self.size_rule {
            SizeRule::PowerOfTwo => {
                size.next_power_of_two()
            }
            SizeRule::MultipleOf(n) => {
                size.next_multiple_of(n)
            }
        }
    }

    fn align_for_stack(&self, addr: u64) -> u64 {
        addr.next_multiple_of(self.stack_align)
    }
}

fn get_target_spec(triple: &str) -> Option<TargetSpec> {
    match triple {
        "thumbv6m-none-eabi" => Some(TargetSpec {
            size_rule: SizeRule::PowerOfTwo,
            alloc_minimum: 32,
            stack_align: 8,
            bfd_name: "armelf".to_string(),
        }),
        "thumbv8m.main-none-eabihf" => Some(TargetSpec {
            size_rule: SizeRule::MultipleOf(32),
            alloc_minimum: 32,
            stack_align: 8,
            bfd_name: "armelf".to_string(),
        }),
        _ => None,
    }
}

#[derive(Clone, Debug, Default)]
struct Allocations {
    tasks: IndexMap<String, BTreeMap<String, TaskAllocation>>,
    kernel: KernelAllocation,
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
struct TaskAllocation {
    requested: u64,
    actual: RangeInclusive<u64>,
}

#[derive(Clone, Debug)]
struct KernelAllocation {
    by_region: BTreeMap<String, RangeInclusive<u64>>,
    stack: RangeInclusive<u64>,
}

impl Default for KernelAllocation {
    fn default() -> Self {
        Self { by_region: Default::default(), stack: 0..=0 }
    }
}

fn allocate_space(
    target_spec: &TargetSpec,
    available: &IndexMap<String, tools::appcfg::Spanned<RegionDef>>,
    required: &BTreeMap<String, IndexMap<&str, u64>>,
    kernel: &KernelDef,
) -> miette::Result<Allocations> {
    let mut results = Allocations::default();

    for (region_name, region) in available {
        let Some(reqs) = required.get(region_name.as_str()) else {
            continue;
        };
        let is_ram = region_name.eq_ignore_ascii_case("RAM");
        // Sort requests for this region in descending size order. TODO: this is
        // an approximation of the right approach.
        let mut reqs = reqs.iter().collect::<Vec<_>>();
        reqs.sort_by_key(|(_name, size)| *size);
        reqs.reverse();

        // Attempt to satisfy each request.
        let orig_addr = *region.value().base.value();
        let end = orig_addr + *region.value().size.value();
        let mut addr = target_spec.align_before_allocation(orig_addr);
        if is_ram {
            addr = target_spec.align_for_stack(addr);
            results.kernel.stack = addr..=addr + (kernel.stack_size.value() - 1);
            addr += kernel.stack_size.value();
            addr = target_spec.align_for_stack(addr);
        }
        if orig_addr != addr {
            eprintln!("warning: adjusted region {region_name} base up to {addr:#x} from {orig_addr:#x}");
        }

        for (taskname, requested_size) in reqs {
            if *requested_size == 0 {
                continue;
            }
            let size = target_spec.round_allocation_size(*requested_size);
            addr = target_spec.align_for_allocation_size(addr, size);
            results.tasks.entry(taskname.to_string())
                .or_default()
                .insert(region_name.clone(), TaskAllocation {
                    requested: *requested_size,
                    actual: addr..=addr + (size - 1),
                });
            addr += size;
        }

        results.kernel.by_region.insert(region_name.to_string(), addr..=end - 1);
    }

    Ok(results)
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
    linker_script: &Path,
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

            let mut installroot = tmpdir.join(format!("{product_name}.cargo-install"));
            let targetdir = tmpdir.join(format!("{product_name}.cargo-target"));

            cmd.arg("--root");
            cmd.arg(&installroot);

            cmd.env("CARGO_TARGET_DIR", targetdir);

            installroot.push("bin");
            installroot.push(&plan.bin_name);
            installroot
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

    println!("{cmd:?}");

    std::fs::copy(
        linker_script,
        &linker_script_copy,
    ).into_diagnostic()?;

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
                .find(|(_name, reg)| *reg.value().base.value() <= phdr.p_vaddr && (phdr.p_paddr + phdr.p_filesz) <= (reg.value().base.value() + reg.value().size.value()))
                .expect("internal inconsistency");

            let sz = region_sizes.get_mut(regname).expect("internal inconsistency");
            sz.start = u64::min(sz.start, phdr.p_paddr);
            sz.end = u64::min(sz.end, phdr.p_paddr + phdr.p_filesz);
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

