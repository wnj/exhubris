//! Basic USB device driver, somewhat specialized for HID right now.
//!
//! # Operation
//!
//! We configure the USB hardware to generate interrupts and then chill.
//!
//! As interrupts arrive, we handle the high-level USB protocol state machine,
//! which includes:
//!
//! - Device reset.
//! - Delivering device, configuration, and string descriptors on demand.
//! - Processing the `SET_CONFIGURATION` operation and a handful of basic HID
//!   operations (like `SET_IDLE`).
//!
//! For most other operations, we defer to a partner task, the keyboard task.
//! The keyboard task is responsible for
//!
//! - Producing the HID report descriptor on demand.
//! - Producing HID reports, as described by that descriptor.
//!
//! When we need the keyboard task to do something, we poke it by posting a
//! (configurable) notification. It then calls back through our IPC interface to
//! make things happen.
//!
//! In ASCII art form:
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

mod usbsram;
mod protocol;
mod hid;

use core::mem::MaybeUninit;
use core::ptr::addr_of_mut;
use core::sync::atomic::{AtomicUsize, Ordering};

use hid::{HidClassDescriptorType, HidRequestCode};
use idyll_runtime::{Leased, Read, Meta};
use num_traits::FromPrimitive as _;
use protocol::{Dir, Recipient, RequestTypeType, StdRequestCode};
use userlib::{ReplyFaultReason, TaskId};
use drv_stm32l4_sys_api::{Stm32L4Sys, Port, Function};
use stm32_metapac::usb::vals::{EpType, Stat};

#[export_name = "main"]
fn main() -> ! {
    // Make an IPC client for the SYS task.
    let sys = Stm32L4Sys::from(hubris_task_slots::SLOTS.sys);

    // STM32L4 USB device setup:

    // Turn on clock to USBFS.
    sys.enable_clock(drv_stm32l4_sys_api::PeripheralName::UsbFs);
    // Also turn on the Clock Recovery System that we use to trim the 48MHz
    // oscillator.
    sys.enable_clock(drv_stm32l4_sys_api::PeripheralName::Crs);

    // Expose USB pins.
    for pin in [11, 12] {
        sys.set_pin_alternate_mode(Port::A, pin, Function::AF10);
    }

    let usb = stm32_metapac::USB;

    // Turn on the analog part of the USB block.
    usb.cntr().modify(|w| w.set_pdwn(false));
    // Now we have to wait for Tstartup before the behavior of the digital bits
    // are defined. Tstartup is 1us on this part, so we're looking at 80 cycles.
    cortex_m::asm::delay(80);

    // Release the digital bits from reset.
    usb.cntr().modify(|w| w.set_fres(false));
    // Clear interrupt flags (probably unnecessary, but the datasheet suggests
    // it)
    usb.istr().write(|w| w.0 = 0);
    // Enable pullup on Dp
    usb.bcdr().modify(|w| w.set_dppu(true));

    // Allow some interrupts.
    usb.cntr().modify(|w| {
        // interrupt on correct transmission
        w.set_ctrm(true);
        // interrupt on reset
        w.set_resetm(true);
        // TODO probably want other interrupts here
    });

    // Enable the CRS
    stm32_metapac::CRS.cr().modify(|w| {
        w.set_autotrimen(true);
        w.set_cen(true);
    });

    // Setup endpoint buffers
    usbsram::fill_buffer_descriptor_table(&usb);

    // Create our server type, which will handle interrupts and IPC messages
    // going forward.
    let mut server = Server {
        usb,
        pending_address: None,
        state: DeviceState::Powered,
        keyboard_task: TaskId::gen0(KEYBOARD_TASK_INDEX),
        expected_out: None,
        queued_event: None,
    };
    // Enable our IRQ.
    userlib::sys_enable_irq(hubris_notifications::USB_IRQ);

    // Message/notification processing loop:
    let mut incoming = [MaybeUninit::uninit(); USB_HID_BUFFER_SIZE];
    loop {
        idyll_runtime::dispatch_or_event(
            &mut server,
            hubris_notifications::USB_IRQ,
            &mut incoming,
        );
    }
}

struct Server {
    usb: stm32_metapac::usb::Usb,
    pending_address: Option<u8>,
    state: DeviceState,
    keyboard_task: TaskId,
    expected_out: Option<hid::OutKind>,
    queued_event: Option<UsbEvent>,
}

#[derive(Copy, Clone, Debug, Default)]
enum DeviceState {
    #[default]
    Powered,
    Default,
    Address,
    Configured,
}

// Our "IRQ handler" to respond to notifications.
impl idyll_runtime::NotificationHandler for Server {
    fn handle_notification(&mut self, bits: u32) {
        if bits & hubris_notifications::USB_IRQ != 0 {
            let istr = self.usb.istr().read();

            // Detect and handle link reset.
            if istr.reset() {
                emit(Event::Reset);
                self.device_reset();
                userlib::sys_enable_irq_and_clear_pending(
                    hubris_notifications::USB_IRQ
                );
                return;
            }

            // Respond to start-of-frame every 1ms
            // Note: currently we don't enable an interrupt on this, so it's
            // kind of vestigial. Evaluate. TODO.
            if istr.sof() {
                self.usb.istr().write(|w| {
                    w.0 = !0;
                    w.set_sof(false);
                });
            }

            // "Correct TRansmission" indicates that something got transferred one
            // way or the other.
            if istr.ctr() {
                // CTR cannot be cleared by writing istr as we did above. We have to
                // clear the root condition that's causing it.

                // The ep_id register is 4 bits but we only implement 8 endpoints.
                // Not sure why. Mask it.
                let ep = usize::from(istr.ep_id() & 0x7);

                let eprv = self.usb.epr(ep).read();

                match istr.dir() {
                    stm32_metapac::usb::vals::Dir::FROM => {
                        // OUT or SETUP (transmission)
                       
                        self.usb.epr(ep).modify(|w| {
                            leave_toggles_unchanged(w);
                            w.set_ctr_rx(false);
                        });

                        if eprv.setup() {
                            // SETUP
                            self.on_setup(ep);
                        } else {
                            // OUT
                            self.on_out(ep);
                        }
                    }
                    stm32_metapac::usb::vals::Dir::TO => {
                        // IN (reception)
                        self.usb.epr(ep).modify(|w| {
                            leave_toggles_unchanged(w);
                            w.set_ctr_tx(false);
                        });
                        self.on_in(ep);
                    }
                }
            }

            userlib::sys_enable_irq_and_clear_pending(
                hubris_notifications::USB_IRQ
            );
        }
    }
}

// Implementation of the idyll-generated trait for handling our IPC interface.
impl UsbHid for Server {
    fn enqueue_report(
        &mut self,
        _: Meta,
        endpoint: u8,
        data: Leased<Read, u8>,
    ) -> Result<Result<bool, EnqueueError>, ReplyFaultReason> {
        match endpoint {
            0 | 1 => {
                if data.len() > 0x40 {
                    return Err(ReplyFaultReason::BadLeases);
                }
                // Amortize the syscall overhead a _little bit_ without having
                // to burn 64 bytes of stack.
                let mut xfer_buffer = [0; 8];
                let mut sram_offset = usbsram::get_ep_tx_offset(endpoint as usize);
                for i in (0..data.len()).step_by(8) {
                    let chunk_end = usize::min(data.len(), i + 8);
                    // Yes, LLVM should be able to see that this is < 8, but it
                    // can't, so include this no-op min call to suppress a
                    // bounds check panic below.
                    let chunk_len = usize::min(chunk_end - i, 8);
                    let chunk_buffer = &mut xfer_buffer[..chunk_len];
                    if data.read_range(i, chunk_buffer).is_err() {
                        // caller went away, choose an arbitrary reply fault
                        // code -- it won't be delivered.
                        return Err(ReplyFaultReason::UndefinedOperation);
                    }
                    usbsram::write_bytes(sram_offset, chunk_buffer);
                    sram_offset += chunk_len;
                }
                usbsram::set_ep_tx_count(endpoint as usize, data.len() as u16);
                self.configure_response(endpoint as usize, Stat::VALID, Stat::VALID);
                emit(Event::HidReportReady);
                Ok(Ok(true))
            }
            _ => {
                // lol no
                Err(ReplyFaultReason::BadMessageContents)
            }
        }
    }

    fn get_event(
        &mut self,
        _: Meta,
    ) -> Result<Option<UsbEvent>, ReplyFaultReason> {
        Ok(self.queued_event.take())
    }
}

// Utility functions and factors of the above code.
impl Server {
    fn device_reset(&mut self) {
        // Clear all interrupt flags.
        self.usb.istr().write(|w| {
            w.0 = !0;
            w.set_reset(false);
            w.set_wkup(false);
            w.set_susp(false);
            w.set_sof(false);
            w.set_esof(false);
        });

        // Set up control EP0 for enumeration.
        self.usb.epr(0).modify(|w| {
            let orig = *w;

            w.set_ea(0);
            w.set_ep_type(EpType::CONTROL);
            w.set_ep_kind(false);

            // dtog_tx/dtog_rx are write-1-to-toggle, and we want to leave them
            // clear. This means we can just ... write their value back.

            // stat_tx/stat_rx are also write-1-to-toggle, but we want to set
            // them to VALID.
            w.set_stat_tx(Stat::from_bits(orig.stat_tx().to_bits() ^ Stat::VALID.to_bits()));
            w.set_stat_rx(Stat::from_bits(orig.stat_rx().to_bits() ^ Stat::VALID.to_bits()));
        });

        // Configure hardware to respond to address 0.
        self.usb.daddr().write(|w| {
            w.set_add(0);
            w.set_ef(true);
        });
        self.pending_address = None;
        self.state = DeviceState::Default;
        self.queued_event = Some(UsbEvent::Reset);
    }

    fn on_setup(&mut self, ep: usize) {
        emit(Event::Setup(ep));
        if ep == 0 {
            // Reset any pending address.
            self.pending_address = None;

            // Collect the received packet from USBSRAM.
            let setup: protocol::SetupPacket = usbsram::read(usbsram::get_ep_rx_offset(ep));

            match (setup.request_type.type_(), setup.request_type.recipient()) {
                (RequestTypeType::Standard, Recipient::Device) => {
                    match (setup.request_type.data_phase_direction(), StdRequestCode::from_u8(setup.request)) {
                        (Dir::HostToDevice, Some(StdRequestCode::SetAddress)) => {
                            let addr = setup.value.get() as u8 & 0x7F;
                            self.pending_address = Some(addr);
                            self.valid_tx_response(0, &[], None);
                        }
                        (Dir::DeviceToHost, Some(StdRequestCode::GetDescriptor)) => {
                            let txoff = usbsram::get_ep_tx_offset(ep);
                            emit(Event::GetDescriptor(setup.value.get()));
                            match protocol::prepare_descriptor(&setup, txoff) {
                                Some(len) => {
                                    usbsram::set_ep_tx_count(ep, setup.length.get().min(len as u16));
                                    self.configure_response(ep, Stat::VALID, Stat::VALID);
                                }
                                None => {
                                    emit(Event::UnknownDescriptor);
                                    self.fail(ep);
                                }
                            }
                        }
                        (Dir::HostToDevice, Some(StdRequestCode::SetConfiguration)) => {
                            self.on_set_config(setup.value.get());
                            self.valid_tx_response(ep, &[], None);
                            self.state = DeviceState::Configured;
                        }
                        (x, y) => {
                            emit(Event::UnknownStandard(x, y));
                            self.fail(ep);
                        }
                    }
                }
                (RequestTypeType::Standard, Recipient::Interface) => {
                    self.on_setup_iface_std(&setup);
                }
                (RequestTypeType::Class, Recipient::Interface) => {
                    self.on_setup_iface_class(&setup);
                }
                (x, y) => {
                    emit(Event::UnknownTop(x, y));
                    self.fail(ep);
                }
            }
        } else {
            // TODO setup on other endpoints (not required for HID)
            self.fail(ep);
        }
    }

    fn on_out(&mut self, ep: usize) {
        emit(Event::Out(ep));
        self.on_out_iface(ep);
    }

    fn on_in(&mut self, ep: usize) {
        emit(Event::In(ep));
        if ep == 0 {
            // Completion of a descriptor transfer or empty status phase.
            if let Some(addr) = self.pending_address.take() {
                // We're completing a pending address assignment! Accept the
                // address.
                self.usb.daddr().write(|w| {
                    w.set_ef(true);
                    w.set_add(addr);
                });
                emit(Event::Address(addr));
                self.state = if addr == 0 {
                    // Well, we've been kicked back to default state in a weird
                    // manner, but, ok
                    DeviceState::Default
                } else {
                    DeviceState::Address
                };
            }

            self.configure_response(ep, Stat::VALID, Stat::VALID);
        } else {
            self.on_in_iface(ep);
        }
    }

    fn configure_response(&mut self, ep: usize, tx: Stat, rx: Stat) {
        self.usb.epr(ep).modify(|w| {
            let orig = *w;
            leave_toggles_unchanged(w);
            w.set_stat_tx(Stat::from_bits(orig.stat_tx().to_bits() ^ tx.to_bits()));
            w.set_stat_rx(Stat::from_bits(orig.stat_rx().to_bits() ^ rx.to_bits()));
        });
    }

    fn valid_tx_response(&mut self, ep: usize, bytes: &[u8], limit: Option<u16>) {
        usbsram::write_bytes(usbsram::get_ep_tx_offset(ep), bytes);
        // Update transmittable count. Some requests read only a prefix, usually
        // of a descriptor, so enforce the limit.
        usbsram::set_ep_tx_count(ep, limit.unwrap_or(u16::MAX).min(bytes.len() as u16));
        // Set the hardware to deliver it when needed.
        self.configure_response(ep, Stat::VALID, Stat::VALID);
    }

    fn fail(&mut self, ep: usize) {
        usbsram::set_ep_tx_count(ep, 0);
        self.configure_response(ep, Stat::STALL, Stat::STALL);
    }


    fn on_set_config(&mut self, config: u16) {
        emit(Event::SetConfig(config));
        // Configure EP1 for HID but don't actually load a report yet. We'll NAK
        // until the keyboard task delivers one.
        self.usb.epr(1).modify(|w| {
            let orig = *w;

            w.set_ep_type(EpType::INTERRUPT);
            w.set_ep_kind(false);

            // Leave dtog_tx/dtog_rx so that they clear themselves.

            // Force stat_tx/stat_rx to NAK.
            w.set_stat_tx(Stat::from_bits(orig.stat_tx().to_bits() ^ Stat::NAK.to_bits()));
            w.set_stat_rx(Stat::from_bits(orig.stat_rx().to_bits() ^ Stat::NAK.to_bits()));

            // Respond to endpoint address 1.
            w.set_ea(1);
        });

        self.poke_keyboard_task(UsbEvent::Configured);
    }

    fn poke_keyboard_task(&mut self, event: UsbEvent) {
        emit(Event::PokedKeyboard);
        self.queued_event = Some(event);
        loop {
            match userlib::sys_post(self.keyboard_task, KEYBOARD_NOTIFICATION_MASK) {
                Ok(()) => break,
                Err(dead) => {
                    // Update and try again. Since we're higher priority, this
                    // shouldn't loop more than once.
                    self.keyboard_task =
                        self.keyboard_task.with_generation(dead.new_generation());
                }
            }
        }
    }

    fn on_setup_iface_std(&mut self, setup: &protocol::SetupPacket) {
        match (setup.request_type.data_phase_direction(), StdRequestCode::from_u8(setup.request)) {
            (Dir::DeviceToHost, Some(StdRequestCode::GetDescriptor)) => {
                match HidClassDescriptorType::from_u16(setup.value.get() >> 8) {
                    Some(HidClassDescriptorType::Report) => {
                        emit(Event::HidDescriptor);
                        // HID Report Descriptor
                        self.poke_keyboard_task(UsbEvent::ReportDescriptorNeeded {
                            length: setup.length.get(),
                        });
                    }
                    _ => {
                        // Unknown kind of descriptor.
                        emit(Event::UnknownIfaceDescriptor);
                        self.fail(0);
                    }
                }
            }
            _ => {
                // Unsupported std operation
                emit(Event::UnknownIfaceOperation);
                self.fail(0);
            }
        }
    }

    fn on_setup_iface_class(&mut self, setup: &protocol::SetupPacket) {
         match (setup.request_type.data_phase_direction(), HidRequestCode::from_u8(setup.request)) {
            (Dir::HostToDevice, Some(HidRequestCode::SetIdle)) => {
                emit(Event::HidSetIdle);
                // TODO: plumb this through to keyboard
                self.valid_tx_response(0, &[], None);
            }
            (Dir::HostToDevice, Some(HidRequestCode::SetReport)) => {
                emit(Event::HidSetReport);
                match setup.value.get() {
                    0x02_00 => {
                        self.expected_out = Some(hid::OutKind::SetReport);
                        self.valid_tx_response(0, &[], None);
                    }
                    _ => {
                        self.fail(0);
                    }
                }
            }
            (Dir::HostToDevice, Some(HidRequestCode::SetProtocol)) => {
                // whatever - our report protocol matches the boot protocol so
                // it's all the same to us.
                emit(Event::HidSetProtocol);
                // TODO: plumb this through to keyboard
                self.valid_tx_response(0, &[], None);
            }
            _ => {
                // Unsupported
                emit(Event::UnknownClassOperation);
                self.fail(0);
            }
        }
   }

    fn on_in_iface(&mut self, _ep: usize) {
        // The host has just read a report. Poke the keyboard task to generate
        // another one. Note that the hardware flips the EP to NAK after
        // transmission, we shouldn't have to do that.
        emit(Event::HidReportCollected);
        self.poke_keyboard_task(UsbEvent::ReportNeeded);
    }

    fn on_out_iface(&mut self, ep: usize) {
        if let Some(kind) = self.expected_out.take() {
            match kind {
                hid::OutKind::SetReport => {
                    let off = usbsram::get_ep_rx_offset(ep);
                    let leds = usbsram::read16(off) as u8;
                    // TODO: set the LEDs
                    emit(Event::Leds(leds));
                }
            }
        }
    }
}

/// My least favorite property of this USB Device block: write-one-to-toggle
/// fields in the endpoint register. This zeroes them so that they don't get
/// changed in a read-modify-write.
fn leave_toggles_unchanged(v: &mut stm32_metapac::usb::regs::Epr) {
    v.set_dtog_tx(false);
    v.set_dtog_rx(false);
    v.set_stat_tx(Stat::from_bits(0));
    v.set_stat_rx(Stat::from_bits(0));
}

include!(concat!(env!("OUT_DIR"), "/generated_server.rs"));
include!(concat!(env!("OUT_DIR"), "/usb_config.rs"));

// Debugger visibility stuff follows:

#[no_mangle]
static mut EVENTS: [Event; 128] = [Event::Reset; 128];
#[no_mangle]
static NEXT_EVENT: AtomicUsize = AtomicUsize::new(0);

#[allow(dead_code)]
#[derive(Copy, Clone)]
enum Event {
    Reset,
    Setup(usize),
    Out(usize),
    In(usize),
    UnknownDescriptor,
    Address(u8),
    UnknownStandard(Dir, Option<StdRequestCode>),
    UnknownTop(RequestTypeType, Recipient),
    GetDescriptor(u16),
    SetConfig(u16),
    HidDescriptor,
    UnknownIfaceDescriptor,
    UnknownIfaceOperation,
    UnknownClassOperation,
    HidSetProtocol,
    HidSetReport,
    HidSetIdle,
    HidReportCollected,
    Leds(u8),
    HidReportReady,
    PokedKeyboard,
}

fn emit(event: Event) {
    let i = NEXT_EVENT.load(Ordering::Relaxed);
    let buf = unsafe { &mut *addr_of_mut!(EVENTS) };
    if let Some(slot) = buf.get_mut(i) {
        *slot = event;
        NEXT_EVENT.store(i + 1, Ordering::Relaxed);
    }
}
