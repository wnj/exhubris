//! Fake keyboard engine.

#![no_std]
#![no_main]

use hubris_task_slots::SLOTS;
use drv_stm32l4_usb_api::{UsbHid, UsbEvent};
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
            hubris_notifications::EVENT_READY
            | hubris_notifications::TIMER
        );

        if bits & hubris_notifications::EVENT_READY != 0 {
            if let Some(event) = usb.get_event() {
                let mut deliver_report = false;
                match event {
                    UsbEvent::Reset => {
                        // We're not really tracking protocol state yet, so,
                        // nothing to do here.
                    }
                    UsbEvent::ReportDescriptorNeeded => {
                        // In this case, we do not deliver a report on EP1.
                        // Instead, we're going to deposit a descriptor on EP0.
                        usb.enqueue_report(0, &BOOT_KBD_DESC).ok();
                    }
                    UsbEvent::Configured => {
                        // When we get configured, we want to prepare an initial
                        // report.
                        deliver_report = true;
                    }
                    UsbEvent::ReportNeeded => {
                        // When our last report is consumed, we definitely want
                        // to prepare a new one.
                        deliver_report = true;
                    }
                }
                if deliver_report {
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
            }
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

static BOOT_KBD_DESC: [u8; 62] = [
    0x05, 0x01,       //  Usage Page (Desktop),
    0x09, 0x06,       //  Usage (Keyboard),
    0xA1, 0x01,       //  Collection (Application),
    0x05, 0x07,       //      Usage Page (Keyboard),
    0x19, 0xE0,       //      Usage Minimum (KB Leftcontrol),
    0x29, 0xE7,       //      Usage Maximum (KB Right GUI),
    0x15, 0x00,       //      Logical Minimum (0),
    0x25, 0x01,       //      Logical Maximum (1),
    0x75, 0x01,       //      Report Size (1),
    0x95, 0x08,       //      Report Count (8),
    0x81, 0x02,       //      Input (Variable),
    0x95, 0x01,       //      Report Count (1),
    0x75, 0x08,       //      Report Size (8),
    0x81, 0x01,       //      Input (Constant),
    0x95, 0x03,       //      Report Count (3),
    0x75, 0x01,       //      Report Size (1),
    0x05, 0x08,       //      Usage Page (LED),
    0x19, 0x01,       //      Usage Minimum (01h),
    0x29, 0x03,       //      Usage Maximum (03h),
    0x91, 0x02,       //      Output (Variable),
    0x95, 0x05,       //      Report Count (5),
    0x75, 0x01,       //      Report Size (1),
    0x91, 0x01,       //      Output (Constant),
    0x95, 0x06,       //      Report Count (6),
    0x75, 0x08,       //      Report Size (8),
    0x26, 0xFF, 0x00, //      Logical Maximum (255),
    0x05, 0x07,       //      Usage Page (Keyboard),
    0x19, 0x00,       //      Usage Minimum (None),
    0x29, 0x91,       //      Usage Maximum (KB LANG2),
    0x81, 0x00,       //      Input,
    0xC0              //  End Collection
];

