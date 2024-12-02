//! STM32L4 blinker. It's annoying how system-specific this is right now. Should
//! fix that eventually.

#![no_std]
#![no_main]

use userlib as _;
use hubris_task_slots::SLOTS;
use drv_stm32l4_sys_api::{Stm32L4Sys as Sys, PeripheralName};

#[export_name = "main"]
fn main() -> ! {
    let sys = Sys::from(SLOTS.sys);

    // Make all our pins outputs.
    for (port, pin) in config::PINS {
        sys.set_pin_output(port, pin);
    }

    let start = userlib::sys_get_timer().now;
    const INTERVAL: u64 = 500;
    let mut next = start + INTERVAL;
    loop {
        for (port, pin) in config::PINS {
            sys.toggle_pin(port, pin);
            userlib::sys_set_timer(Some(next), hubris_notifications::TIMER);
            userlib::sys_recv_notification(hubris_notifications::TIMER);
            next += INTERVAL;
        }
    }
}

include!(concat!(env!("OUT_DIR"), "/task_config.rs"));
