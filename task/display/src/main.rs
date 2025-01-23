//! A task that operates as s SPI client.
//!
//! This task exists to test the SPI link.

#![no_std]
#![no_main]

use drv_stm32g0_sys_api::{PeripheralName, Port, Stm32G0Sys as Sys};
use hubris_task_slots::SLOTS;

#[export_name = "main"]
fn main() -> ! {
    // Create a client for the Sys driver and make our LED pin an output.
    let sys = Sys::from(SLOTS.sys);

    sys.enable_clock(PeripheralName::Tim17);

    let tim = stm32_metapac::TIM17;

    //----
    tim.psc().write(|w| w.set_psc(64 - 1)); // 1us
    tim.arr().write(|w| w.set_arr(10));

    // start the timer
    tim.cr1().write(|w| w.set_cen(true));
    //----

    // LED L12
    sys.set_pin_output(Port::A, 0);

    // 7-segment displays
    // CAn = high (off)
    sys.set_pin_output(Port::B, 6); // DP-1 (right most)
    sys.set_pin_high(Port::B, 6);
    sys.set_pin_output(Port::B, 7); // DP-2
    sys.set_pin_high(Port::B, 7);
    sys.set_pin_output(Port::B, 8); // DP-3
    sys.set_pin_high(Port::B, 8);
    sys.set_pin_output(Port::B, 9); // DP-4 (left most)
    sys.set_pin_high(Port::B, 9);
    // segments a..f, dp (off)
    sys.set_pin_output(Port::A, 4); // segment a
    sys.set_pin_high(Port::A, 4);
    sys.set_pin_output(Port::A, 5); // segment b
    sys.set_pin_high(Port::A, 5);
    sys.set_pin_output(Port::A, 6); // segment c
    sys.set_pin_high(Port::A, 6);
    sys.set_pin_output(Port::A, 7); // segment d
    sys.set_pin_high(Port::A, 7);
    sys.set_pin_output(Port::A, 8); // segment e
    sys.set_pin_high(Port::A, 8);
    sys.set_pin_output(Port::A, 9); // segment f
    sys.set_pin_high(Port::A, 9);
    sys.set_pin_output(Port::A, 10); // segment g
    sys.set_pin_high(Port::A, 10);
    sys.set_pin_output(Port::A, 11); // segment dp
    sys.set_pin_high(Port::A, 11);

    for p in 0..12 {
        sys.set_pin_low(Port::A, p);
    }

    // Record the current time so we can start our delay loop properly.
    let mut next_send = userlib::sys_get_timer().now;

    const INTERVAL: u64 = 500; // milliseconds

    let mut display = 0;

    loop {
        userlib::sys_set_timer(Some(next_send), hubris_notifications::TIMER_DISPLAY);

        // The proper thing to do, when waiting for a timer, is to sleep waiting
        // for notifications _and then check the time._ Otherwise other tasks
        // can wake you up by posting.
        loop {
            userlib::sys_recv_notification(hubris_notifications::TIMER_DISPLAY);
            let now = userlib::sys_get_timer().now;
            if now >= next_send {
                next_send += INTERVAL;
                break;
            }
        }

        sys.toggle_pin(Port::A, 0);

        if display == 0 {
            display = 6;
            sys.set_pin_high(Port::B, 6);
            sys.set_pin_high(Port::B, 7);
            sys.set_pin_high(Port::B, 8);
            sys.set_pin_high(Port::B, 9);
        }

        sys.set_pin_low(Port::B, display);

        display += 1;

        if display > 9 {
            display = 0;
        }
    }
}

// include!(concat!(env!("OUT_DIR"), "/generated_server.rs"));
// include!(concat!(env!("OUT_DIR"), "/config.rs"));
