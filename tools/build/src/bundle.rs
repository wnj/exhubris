use std::{collections::{BTreeMap, BTreeSet}, error::Error, fs, io::Write as _, path::{Path, PathBuf}};

use indexmap::{IndexMap, IndexSet};
use serde::Serialize;
use size::Size;

use crate::{appcfg::{self, AppDef}, get_target_spec};

#[derive(Debug, thiserror::Error)]
#[non_exhaustive]
pub enum BundleError {
    #[error("underlying I/O operation failed")]
    Io(#[from] std::io::Error),
    #[error("could not update ZIP file")]
    Zip(#[from] zip::result::ZipError),
    #[error("error reading ELF file from previous build step")]
    Goblin(#[from] goblin::error::Error),
    #[error("error writing combined ELF file")]
    Object(#[from] object::write::Error),

    #[error("build output contains a mix of big- and little-endian objects")]
    MixedEndian,
    #[error("build output contains a mix of 32- and 64-bit objects")]
    MixedPointerWidth,
    #[error("build output contains a mix of OS ABI values")]
    MixedOsAbi,
    #[error("build output contains a mix of ABI version values")]
    MixedAbiVersion,
    #[error("build output contains a mix of machine types")]
    MixedMachine,
}

pub fn make_bundle(
    app: &AppDef,
    bindir: &Path,
    outpath: &Path,
) -> Result<(), BundleError> {
    let f = std::fs::File::create(outpath)?;
    let mut z = zip::ZipWriter::new(f);
    let opts = zip::write::SimpleFileOptions::default();

    z.set_comment("hubris build archive v9");

    {
        z.start_file("app.toml", opts)?;

        let tasks_toml = app.tasks.iter().map(|(name, task)| {
            (name.clone(), TaskToml {
                notifications: task.notifications.clone(),
            })
        }).collect();

        let app_toml = AppToml {
            name: app.name.value().clone(),
            target: app.board.chip.target_triple.value().clone(),
            board: app.board.name.value().clone(),
            kernel: KernelToml {
                name: match app.kernel.package_source.value() {
                    appcfg::PackageSource::WorkspaceCrate { name } => name.clone(),
                    appcfg::PackageSource::GitCrate { name, .. } => name.clone(),
                },
            },
            tasks: tasks_toml,
        };
        let toml_text = toml::to_string(&app_toml).unwrap();
        writeln!(z, "# stub TOML generated because Humility expects it")?;
        z.write_all(toml_text.as_bytes())?;
    }

    let mut collected_segments = BTreeMap::new();

    // We expect these properties to be consistent across the ELF files, but we
    // collect them into sets to find out if they aren't.
    let mut little_endian = BTreeSet::new();
    let mut is_64 = BTreeSet::new();
    let mut elf_machine = BTreeSet::new();
    let mut elf_os_abi = BTreeSet::new();
    let mut elf_abi_version = BTreeSet::new();

    let mut kernel_entry = None;

    if let Some(prs_name) = &app.board.chip.probe_rs_name {
        let text = ron::to_string(&FlashRon {
            chip: Some(prs_name.value().clone()),
        }).unwrap();
        z.start_file("img/flash.ron", opts)
            ?;
        z.write_all(text.as_bytes())?;
    }

    let names = app.tasks.keys()
        .map(|s| s.as_str())
        .chain(std::iter::once("kernel"));

    for name in names {
        let is_the_kernel = name == "kernel";

        let archive_path = if is_the_kernel {
            "elf/kernel".to_string()
        } else {
            format!("elf/task/{name}")
        };

        let elfpath = bindir.join(name);
        let elf_bytes = std::fs::read(&elfpath)
            ?;
        z.start_file(archive_path, opts)
            ?;
        z.write_all(&elf_bytes)?;

        let elf = goblin::elf::Elf::parse(&elf_bytes)?;
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

        if is_the_kernel {
            kernel_entry = Some(elf.entry);
        }
    }

    let base_addr = *collected_segments.first_key_value().unwrap().0;
    let (_final_addr, flattened) = collected_segments.into_iter()
        .fold((None, vec![]), |(last_addr, mut flattened), (addr, bytes)| {
            if let Some(la) = last_addr {
                let gap_size = addr - la;
                let new_len = flattened.len() + usize::try_from(gap_size).unwrap();
                flattened.resize(new_len, 0xFF);
            }
            let n = bytes.len();
            flattened.extend(bytes);

            (Some(addr + u64::try_from(n).unwrap()), flattened)
        });

    if little_endian.len() == 2 {
        return Err(BundleError::MixedEndian);
    }
    let little_endian = little_endian.pop_last().unwrap();
    if is_64.len() == 2 {
        return Err(BundleError::MixedPointerWidth);
    }
    let is_64 = is_64.pop_last().unwrap();
    if elf_os_abi.len() == 2 {
        return Err(BundleError::MixedOsAbi);
    }
    let elf_os_abi = elf_os_abi.pop_last().unwrap();
    if elf_abi_version.len() == 2 {
        return Err(BundleError::MixedAbiVersion);
    }
    let elf_abi_version = elf_abi_version.pop_last().unwrap();
    if elf_machine.len() == 2 {
        return Err(BundleError::MixedMachine);
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
    })?;
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
        ?;
    z.write_all(&final_buf)?;

    drop(z);
    Ok(())
}

#[derive(Serialize)]
pub struct AppToml {
    pub target: String,
    pub name: String,
    pub board: String,

    pub kernel: KernelToml,

    pub tasks: IndexMap<String, TaskToml>,
}

#[derive(Serialize)]
pub struct KernelToml {
    pub name: String
}

#[derive(Serialize)]
pub struct TaskToml {
    pub notifications: IndexSet<String>,
}


#[derive(Serialize)]
pub struct FlashRon {
    pub chip: Option<String>,
}

