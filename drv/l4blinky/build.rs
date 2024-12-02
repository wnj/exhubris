use std::path::PathBuf;
use std::io::Write;

use serde_json::{Map, Value};

fn main() {
    println!("cargo::rerun-if-env-changed=HUBRIS_TASK_CONFIG");
    let Ok(configstr) = std::env::var("HUBRIS_TASK_CONFIG") else {
        panic!("Missing config for task!");
    };
    let config: Map<String, Value> = serde_json::from_str(&configstr).unwrap();

    let mut out = PathBuf::from(std::env::var("OUT_DIR").unwrap());
    out.push("task_config.rs");

    let mut f = std::fs::File::create(&out).unwrap();

    writeln!(f, "pub(crate) mod config {{").unwrap();
    writeln!(f, "use drv_stm32l4_sys_api::Port;").unwrap();

    let pins = config["pins"].as_array().unwrap();
    let npins = pins.len();
    writeln!(f, "pub const PINS: [(Port, u8); {npins}] = [").unwrap();
    for pinname in pins {
        let (port, pin) = parse_pin_name(pinname.as_str().unwrap());
        writeln!(f, "    (Port::{port}, {pin}),").unwrap();
    }
    writeln!(f, "];").unwrap();
    writeln!(f, "}}").unwrap();
}

fn parse_pin_name(name: &str) -> (&str, u8) {
    let (port, pin) = name.split_at(1);

    (port, pin.parse().unwrap())
}
