use std::path::PathBuf;

fn main() {
    println!("cargo::rerun-if-changed=../stm32l4-usb/usbhid-idl.kdl");
    let iface = tools::idl::load_interface("../stm32l4-usb/usbhid-idl.kdl").unwrap();
    let client = tools::idl::codegen::generate_client(&iface).unwrap();
    let client = tools::idl::codegen::format_code(&client);
    let mut outpath = PathBuf::from(std::env::var("OUT_DIR").unwrap());
    outpath.push("generated_client.rs");

    std::fs::write(&outpath, client).unwrap();

}