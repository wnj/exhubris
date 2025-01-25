use std::path::PathBuf;
use std::io::Write;

use serde_json::{Map, Value};

fn main() {
    let config: Map<String, Value> = hubris_build_util::get_task_config();

    let mut out = PathBuf::from(std::env::var("OUT_DIR").unwrap());
    out.push("task_config.rs");

    let mut f = std::fs::File::create(&out).unwrap();

    writeln!(f, "pub(crate) mod config {{").unwrap();
    writeln!(f, "use drv_stm32xx_sys_api::Port;").unwrap();

    let (port, pin) = parse_pin_name(config["led-pin"].as_str().unwrap());
    writeln!(f, "pub const PIN: (Port, u8) = (Port::{port}, {pin});").unwrap();
    writeln!(f, "}}").unwrap();
}

fn parse_pin_name(name: &str) -> (&str, u8) {
    let (port, pin) = name.split_at(1);

    (port, pin.parse().unwrap())
}

