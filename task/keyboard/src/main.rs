//! Fake keyboard engine.

#![no_std]
#![no_main]

use hubris_task_slots::SLOTS;
use drv_stm32l4_usb_api::UsbHid;
use drv_stm32l4_sys_api::{Stm32L4Sys as Sys, Port, Pull};
use core::sync::atomic::{AtomicUsize, Ordering};

#[no_mangle]
static REPORTS: AtomicUsize = AtomicUsize::new(0);

#[export_name = "main"]
fn main() -> ! {
    let usb = UsbHid::from(SLOTS.usbhid);
    let sys = Sys::from(SLOTS.sys);

    sys.set_pin_output(Port::A, 0);
    sys.set_pin_output(Port::A, 1);

    sys.set_pin_pull(Port::B, 0, Some(Pull::Down));
    sys.set_pin_pull(Port::B, 1, Some(Pull::Down));
    sys.set_pin_input(Port::B, 0);
    sys.set_pin_input(Port::B, 1);

    // Record the current time so we can manage our timer properly.
    let mut next_time = userlib::sys_get_timer().now + INTERVAL;
    userlib::sys_set_timer(Some(next_time), hubris_notifications::TIMER);

    const INTERVAL: u64 = 1;

    let mut scan_state = 0;
    let key_codes = [0x04, 0x05, 0x06, 0x07];
    let mut keys_down = [false; 4];

    loop {
        let bits = userlib::sys_recv_notification(
            hubris_notifications::REPORT_NEEDED
            | hubris_notifications::TIMER
        );

        if bits & hubris_notifications::REPORT_NEEDED != 0 {
            let mut report = [0u8; 8];
            let mut used = 2;
            for (i, state) in keys_down.iter_mut().enumerate() {
                if *state {
                    report[used] = key_codes[i];
                    used += 1;
                }
            }
            usb.enqueue_report(1, &report).ok();
            REPORTS.fetch_add(1, Ordering::Relaxed);
        }
        if bits & hubris_notifications::TIMER != 0 {
            let now = userlib::sys_get_timer().now;
            if now >= next_time {
                // A real timer event!
                next_time += INTERVAL;

                match scan_state {
                    0 => {
                        keys_down[0] = sys.is_pin_high(Port::B, 0);
                        keys_down[1] = sys.is_pin_high(Port::B, 1);
                        sys.set_pin_low(Port::A, 1);
                        sys.set_pin_high(Port::A, 0);
                        scan_state = 1;
                    }
                    _ => {
                        keys_down[2] = sys.is_pin_high(Port::B, 0);
                        keys_down[3] = sys.is_pin_high(Port::B, 1);
                        sys.set_pin_low(Port::A, 0);
                        sys.set_pin_high(Port::A, 1);
                        scan_state = 0;
                    }
                }
                userlib::sys_set_timer(Some(next_time), hubris_notifications::TIMER);
            }
        }
    }
}
