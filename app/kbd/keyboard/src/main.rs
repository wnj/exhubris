//! Basic USB HID keyboard engine.
//!
//! This task manages key matrix scanning and report construction, driven by the
//! OS timer and the USB task.
//!
//! # Operation
//!
//! This task runs a continuous event loop, driven by its OS timer, that sets
//! one SCANOUT line high at a time, and then reads the state of all SCANIN
//! lines. Because the keyboard in question is tiny (four keys) I'm just doing
//! this in software at a relatively low polling rate (500 Hz). Key states are
//! tracked in a basic array of `bool`s on the stack. I'm not debouncing at the
//! moment because this is a proof of concept.
//!
//! The other events come from the USB task. The USB task doesn't interact with
//! us until the host has set up and configured the USB device. At that point,
//! we start receiving events that we need to respond to.
//!
//! The basic structure of any USB event is as follows:
//!
//! 1. The USB task posts our `USB_EVENT_READY` notification, finishes up
//!    whatever it was doing, and goes to sleep.
//! 2. We wake up and notice the notification. In response, we send a
//!    `get_event` message to the USB task.
//! 3. The USB task wakes up to receive the event and replies with a `UsbEvent`
//!    describing the condition we need to handle. It then goes back to sleep.
//! 4. We receive the reply and take action. Some events currently require no
//!    action, but most require us to furnish the USB task with either our
//!    report descriptor, or the next report. We deliver these by sending the
//!    USB task an `enqueue_report` message, loaning the appropriate section of
//!    our memory along with it.
//! 5. The USB task wakes to receive the `enqueue_report` message and copies our
//!    loaned data into the USB controller's SRAM. It configures the USB
//!    controller to transmit that data on the next IN operation on that
//!    endpoint, replies to us (with an empty message), and goes back to sleep.
//! 6. Receiving our reply, we go back to the top of the event loop and go to
//!    sleep.
//!
//! Or in ASCII art form,
//!
//! ```text
//! +-----------+          +---------+                         +-----------+
//! | Hardware  |          | USBHID  |                         | Keyboard  |
//! +-----------+          +---------+                         +-----------+
//!       |                     |                                    |
//!       | IRQ                 |                                    |
//!       |-------------------->|                                    |
//!       |                     |                      ------------\ |
//!       |                     |                      | is asleep |-|
//!       |                     |                      |-----------| |
//!       |                     |                                    |
//!       |                     | posts EVENT notification.          |
//!       |                     |----------------------------------->|
//!       |                     | ---------------------\             |
//!       |                     |-| goes back to sleep |             |
//!       |                     | |--------------------|             |
//!       |                     |                      ------------\ |
//!       |                     |                      | wakes up! |-|
//!       |                     |                      |-----------| |
//!       |                     |                                    |
//!       |                     |           sends GET_EVENT message. |
//!       |                     |<-----------------------------------|
//!       |                     |                                    |
//!       |                     | returns the event.                 |
//!       |                     |----------------------------------->|
//!       |                     | ---------------------\             |
//!       |                     |-| goes back to sleep |             |
//!       |                     | |--------------------|             |
//!       |                     |                                    |
//!       |                     |       sends ENQUEUE_REPORT message |
//!       |                     |<-----------------------------------|
//!       |                     | -----------------------\           |
//!       |                     |-| copies into USB SRAM |           |
//!       |                     | |----------------------|           |
//!       |                     |                                    |
//!       |      endpoint ready |                                    |
//!       |<--------------------|                                    |
//!       |                     |                                    |
//!       |                     | empty reply                        |
//!       |                     |----------------------------------->|
//!       |                     | ---------------------\             |
//!       |                     |-| goes back to sleep |             |
//!       |                     | |--------------------|             |
//!       |                     |        --------------------------\ |
//!       |                     |        | also goes back to sleep |-|
//!       |                     |        |-------------------------| |
//!       |                     |                                    |
//! ```

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

