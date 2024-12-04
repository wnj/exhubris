//! Fake keyboard engine.

#![no_std]
#![no_main]

use hubris_task_slots::SLOTS;
use drv_stm32l4_usb_api::UsbHid;
use core::sync::atomic::{AtomicUsize, Ordering};

#[no_mangle]
static REPORTS: AtomicUsize = AtomicUsize::new(0);

#[export_name = "main"]
fn main() -> ! {
    let usb = UsbHid::from(SLOTS.usbhid);

    // Record the current time so we can start our delay loop properly.
    let mut next_send = userlib::sys_get_timer().now;

    const INTERVAL: u64 = 1000;

    loop {
        userlib::sys_recv_notification(hubris_notifications::REPORT_NEEDED);
        let now = userlib::sys_get_timer().now;
        let report = if now >= next_send {
            next_send += INTERVAL;
            &[0, 0, 0x04, 0, 0, 0, 0, 0]
        } else {
            &[0, 0, 0, 0, 0, 0, 0, 0]
        };
        usb.enqueue_report(1, report).ok();
        REPORTS.fetch_add(1, Ordering::Relaxed);
    }
}
