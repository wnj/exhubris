use std::path::PathBuf;

fn main() {
    println!("cargo::rerun-if-changed=usbhid-idl.kdl");
    let iface = tools::idl::load_interface("usbhid-idl.kdl").unwrap();
    let server = tools::idl::codegen::generate_server(&iface).unwrap();
    let server = tools::idl::codegen::format_code(&server);
    let mut outpath = PathBuf::from(std::env::var("OUT_DIR").unwrap());
    outpath.push("generated_server.rs");

    std::fs::write(&outpath, server).unwrap();

}
