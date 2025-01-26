//! A task that blinks a LED at PC13.
//!
//! This task exists to test the build system.

#![no_std]
#![no_main]

use drv_stm32xx_sys_api::{Port, Stm32Sys as Sys};
use hubris_task_slots::SLOTS;

#[export_name = "main"]
fn main() -> ! {
    // Create a client for the Sys driver and make our LED pin an output.
    let sys = Sys::from(SLOTS.sys);
    sys.set_pin_output(Port::C, 15); // GREEN led

    // Record the current time so we can start our delay loop properly.
    let mut next_send = userlib::sys_get_timer().now;

    const INTERVAL: u64 = 1000; // milliseconds

    loop {
        userlib::sys_set_timer(Some(next_send), hubris_notifications::TIMER_BLINKY);

        // The proper thing to do, when waiting for a timer, is to sleep waiting
        // for notifications _and then check the time._ Otherwise other tasks
        // can wake you up by posting.
        loop {
            userlib::sys_recv_notification(hubris_notifications::TIMER_BLINKY);
            let now = userlib::sys_get_timer().now;
            if now >= next_send {
                next_send += INTERVAL;
                break;
            }
        }

        sys.toggle_pin(Port::C, 15);
    }
}
