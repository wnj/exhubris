pub mod appcfg;
pub mod alloc;
pub mod idl;
pub mod config;

use std::{path::PathBuf, process::Command};

use cargo_metadata::Package;
use miette::{bail, miette, IntoDiagnostic as _};

#[derive(Clone, Debug)]
pub struct BuildEnv {
    /// release of toolchain
    pub release: String,
    /// sysroot for toolchain obtained from rustup.
    pub sysroot: PathBuf,
    /// Host triple --- the triple used for tools that run on the host, rather
    /// than being cross-compiled.
    pub host_triple: String,
    /// The location we have inferred for the linker.
    pub linker_path: PathBuf,
}

pub fn determine_build_env() -> miette::Result<BuildEnv> {
    let sysroot = Command::new("rustc")
        .args(["--print", "sysroot"])
        .output().into_diagnostic()?;
    if !sysroot.status.success() {
        panic!("Could not find execute rustc to get sysroot");
    }
    let sysroot = PathBuf::from(std::str::from_utf8(&sysroot.stdout).into_diagnostic()?.trim());

    let host = Command::new(sysroot.join("bin").join("rustc"))
        .arg("-vV")
        .output().into_diagnostic()?;
    if !host.status.success() {
        panic!("Could not execute rustc to get host");
    }
    let output = std::str::from_utf8(&host.stdout).into_diagnostic()?;
    let host_triple = output.lines()
        .find_map(|line| line.strip_prefix("host: "))
        .ok_or_else(|| miette!("Could not get host from rustc"))?
        .to_string();
    let release = output.lines()
        .find_map(|line| line.strip_prefix("release: "))
        .ok_or_else(|| miette!("Could not get release from rustc"))?
        .to_string();

    let mut linker_path = sysroot.clone();
    linker_path.extend([
        "lib",
        "rustlib",
        &host_triple,
        "bin",
        "gcc-ld",
        "ld.lld",
    ]);
    if !std::fs::exists(&linker_path).into_diagnostic()? {
        bail!("linker not available at: {}", linker_path.display());
    }

    Ok(BuildEnv {
        release,
        sysroot,
        host_triple,
        linker_path,
    })
}

pub enum BuildablePackage {
    WorkspaceCrate {
        name: String,
        /// The entire package metadata record, proving that we found it (and
        /// providing useful contents).
        package: Package,
        /// The cargo arguments to build the crate.
        cargo_args: Vec<String>,
        /// The path where we expect to find the result.
        product_path: PathBuf,
    },
}

#[derive(Copy, Clone, Debug)]
pub enum SizeRule {
    /// Allocations must be a power-of-two in size.
    PowerOfTwo,
    /// Allocations must be an integer multiple of this number.
    MultipleOf(u64),
}

pub struct TargetSpec {
    /// Rule determining legal allocation sizes on this target.
    pub size_rule: SizeRule,
    /// Minimum allocation size on this target.
    pub alloc_minimum: u64,
    /// Stack alignment requirement in bytes.
    pub stack_align: u64,
    /// BFD architecture name.
    pub bfd_name: String,
}

impl TargetSpec {
    pub fn align_before_allocation(&self, addr: u64) -> u64 {
        addr.next_multiple_of(self.alloc_minimum)
    }

    pub fn align_for_allocation_size(&self, addr: u64, size: u64) -> u64 {
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

    pub fn align_to_next_larger_boundary(&self, addr: u64) -> u64 {
        match self.size_rule {
            SizeRule::PowerOfTwo => {
                addr + (1 << addr.trailing_zeros())
            }
            SizeRule::MultipleOf(n) => {
                addr.next_multiple_of(n)
            }
        }
    }

    pub fn round_allocation_size(&self, size: u64) -> u64 {
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

    pub fn align_for_stack(&self, addr: u64) -> u64 {
        addr.next_multiple_of(self.stack_align)
    }
}

pub fn get_target_spec(triple: &str) -> Option<TargetSpec> {
    match triple {
        "thumbv6m-none-eabi" => Some(TargetSpec {
            size_rule: SizeRule::PowerOfTwo,
            alloc_minimum: 32,
            stack_align: 8,
            bfd_name: "armelf".to_string(),
        }),
        "thumbv7em-none-eabihf" => Some(TargetSpec {
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


