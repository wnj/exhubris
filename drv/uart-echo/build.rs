use std::path::PathBuf;
use std::io::Write;

use serde_json::{Map, Value};

fn main() {
    let config: Map<String, Value> = hubris_build_util::get_task_config();

    let mut out = PathBuf::from(std::env::var("OUT_DIR").unwrap());
    out.push("task_config.rs");

    let mut f = std::fs::File::create(&out).unwrap();

    writeln!(f, "pub(crate) mod config {{").unwrap();
    writeln!(f, "use drv_stm32g0_sys_api::{{Port, Function}};").unwrap();

    writeln!(f, "pub const UART_CLOCK_HZ: u32 = {};", config["uart-clock-hz"]).unwrap();
    writeln!(f, "pub const BAUD_RATE: u32 = {};", config["baud-rate"]).unwrap();

    writeln!(f, "pub const PINS: [(Port, u8, Function); 2] = [").unwrap();
    for pinaf in config["pins"].as_array().unwrap() {
        let (port, pin) = parse_pin_name(pinaf["name"].as_str().unwrap());
        writeln!(f, "    (Port::{port}, {pin}, Function::AF{}),", pinaf["af"].as_u64().unwrap()).unwrap();
    }
    writeln!(f, "];").unwrap();
    writeln!(f, "}}").unwrap();
}

fn parse_pin_name(name: &str) -> (&str, u8) {
    let (port, pin) = name.split_at(1);

    (port, pin.parse().unwrap())
}
