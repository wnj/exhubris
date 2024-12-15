use std::io::Write;
use std::path::PathBuf;

use serde::Deserialize;

fn main() {
    let outpath = PathBuf::from(std::env::var("OUT_DIR").unwrap());

    println!("cargo::rerun-if-changed=scanner-idl.kdl");
    let iface = hubris_build::idl::load_interface("scanner-idl.kdl").unwrap();
    let server = hubris_build::idl::codegen::generate_server(&iface).unwrap();
    let server = hubris_build::idl::codegen::format_code(&server);
    let genserver_path = outpath.join("generated_server.rs");

    std::fs::write(&genserver_path, server).unwrap();

    let config: Config = hubris_build_util::get_task_config();

    let mut f = std::fs::File::create(outpath.join("config.rs")).unwrap();
    writeln!(f, "pub(crate) mod config {{").unwrap();

    writeln!(f, "use drv_stm32l4_sys_api::Port;").unwrap();

    for (array, src) in [("ROWS", &config.rows), ("COLS", &config.cols)] {
        let n = src.len();
        writeln!(f, "pub const {array}: [(Port, u8); {n}] = [").unwrap();
        for pinname in src {
            let (port, pin) = parse_pin_name(pinname);
            writeln!(f, "    (Port::{port}, {pin}),").unwrap();
        }
        writeln!(f, "];").unwrap();
    }

    writeln!(f, "pub const ROW_COUNT: usize = {};", config.rows.len()).unwrap();
    writeln!(f, "pub const COL_COUNT: usize = {};", config.cols.len()).unwrap();

    writeln!(f, "}}").unwrap();
    drop(f);
}

#[derive(Deserialize)]
struct Config {
    rows: Vec<String>,
    cols: Vec<String>,
}

fn parse_pin_name(name: &str) -> (&str, u8) {
    let (port, pin) = name.split_at(1);

    (port, pin.parse().unwrap())
}
