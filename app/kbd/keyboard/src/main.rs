//! Basic USB HID keyboard engine.
//!
//! This task maintains _logical_ keyboard state and constructs USB HID reports
//! that reflect it. (This is distinct from _physical_ keyboard state, which is
//! the state of the key matrix.)
//!
//! This task has two collaborator tasks:
//!
//! - `usbhid` for actually delivering HID reports and learning about USB
//!   protocol state machine transitions.
//!
//! - `scanner` for monitoring the key matrix and detecting transitions.
//!
//! We basically only act in response to events from one of those collaborators.
//! Both of our collaborators have response time requirements, so they will
//! normally be higher priority than this task and cannot message us directly.
//! As a result, interaction is "backwards" through the pingback pattern.
//!
//! Concretely, when a collaborator decides that they have something to tell us,
//! they post a notification to this task. This task wakes up, notices the
//! notification, and sends a message to the corresponding collaborator to find
//! out what happened.
//!
//! This has one more syscall round-trip than the simple pattern of delivering
//! messages directly to this task, but ensures that the two collaborator tasks
//! can't block each other by accident, and that this task can't block either of
//! them.

#![no_std]
#![no_main]

use hubris_task_slots::SLOTS;
use drv_stm32l4_usb_api::{UsbHid, UsbEvent};
use drv_stm32l4_sys_api::{Stm32L4Sys as Sys, Port, Pull};
use kbd_basic_scanner_api::{KeyEvent, KeyState, Scanner};
use core::sync::atomic::{AtomicUsize, Ordering};

/// Counter variable for visibility in the debugger.
#[no_mangle]
static REPORTS: AtomicUsize = AtomicUsize::new(0);

#[export_name = "main"]
fn main() -> ! {
    // Make IPC clients for the two tasks we interact with.
    let usb = UsbHid::from(SLOTS.usbhid);
    let scanner = Scanner::from(SLOTS.scanner);

    // Local variables for matrix scanning and key state:
    let key_codes = [0x04, 0x05, 0x06, 0x07];
    let mut keys_down = [false; 4];

    // Event loop!
    loop {
        // We are interested in these events:
        let bits = userlib::sys_recv_notification(
            hubris_notifications::USB_EVENT_READY
            | hubris_notifications::SCAN_EVENT_READY
        );

        if bits & hubris_notifications::USB_EVENT_READY != 0 {
            if let Some(event) = usb.get_event() {
                let mut deliver_report = false;
                match event {
                    UsbEvent::Reset => {
                        // We're not really tracking protocol state yet, so,
                        // nothing to do here.
                    }
                    UsbEvent::ReportDescriptorNeeded { length } => {
                        // In this case, we do not deliver a report on EP1.
                        // Instead, we're going to deposit a descriptor on EP0.
                        let n = BOOT_KBD_DESC.len().min(usize::from(length));
                        usb.enqueue_report(0, &BOOT_KBD_DESC[..n]).ok();
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
                    // Construct a HID Boot Keyboard Protocol report.
                    let mut report = [0u8; 8];
                    let mut used = 2;
                    for (i, state) in keys_down.iter_mut().enumerate() {
                        if *state {
                            report[used] = key_codes[i];
                            used += 1;
                        }
                    }
                    // Stuff it into endpoint 1's outgoing buffer.
                    usb.enqueue_report(1, &report).ok();
                    REPORTS.fetch_add(1, Ordering::Relaxed);
                }
            }
        }

        if bits & hubris_notifications::SCAN_EVENT_READY != 0 {
            if let Some(event) = scanner.pop_event() {
                if let Some(status) = keys_down.get_mut(usize::from(event.row * 2 + event.col)) {
                    match event.state {
                        KeyState::Down => *status = true,
                        KeyState::Up => *status = false,
                    }
                } else {
                    // out of range for our matrix
                }
            }
        }
    }
}

/// Canned report descriptor that describes a report equivalent to the Boot
/// Keyboard Protocol report.
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

