#![no_std]
#![no_main]

use userlib as _;

#[export_name = "main"]
fn main() -> ! {
    loop {
        userlib::idle();
    }
}
