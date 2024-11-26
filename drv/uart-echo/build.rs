use std::path::PathBuf;
use std::io::Write;

use serde::Deserialize;

fn main() {
    println!("cargo::rerun-if-env-changed=HUBRIS_TASK_CONFIG");
    let Ok(configstr) = std::env::var("HUBRIS_TASK_CONFIG") else {
        panic!("Missing config for task!");
    };
    let config: Config = serde_json::from_str(&configstr).unwrap();

    let mut out = PathBuf::from(std::env::var("OUT_DIR").unwrap());
    out.push("task_config.rs");

    let mut f = std::fs::File::create(&out).unwrap();

    writeln!(f, "pub(crate) mod config {{").unwrap();
    writeln!(f, "use drv_stm32g0_sys_api::Port;").unwrap();

    writeln!(f, "pub const UART_CLOCK_HZ: u32 = {};", config.uart_clock_hz).unwrap();
    writeln!(f, "pub const BAUD_RATE: u32 = {};", config.baud_rate).unwrap();

    writeln!(f, "pub const PINS: [(Port, u8, u8); 2] = [").unwrap();
    for pinaf in &config.pins {
        let (port, pin) = parse_pin_name(&pinaf.name);
        writeln!(f, "    (Port::{port}, {pin}, {}),", pinaf.af).unwrap();
    }
    writeln!(f, "];").unwrap();
    writeln!(f, "}}").unwrap();
}

fn parse_pin_name(name: &str) -> (&str, u8) {
    let (port, pin) = name.split_at(1);

    (port, pin.parse().unwrap())
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
struct Config {
    uart_clock_hz: u32,
    baud_rate: u32,
    pins: [PinAf; 2],
}

#[derive(Debug, Deserialize)]
struct PinAf {
    name: String,
    af: u8,
}
