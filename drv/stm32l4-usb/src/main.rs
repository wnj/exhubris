#![no_std]
#![no_main]

mod usbsram;
mod protocol;

use core::mem::MaybeUninit;
use core::ptr::addr_of_mut;
use core::sync::atomic::{AtomicUsize, Ordering, AtomicU8};

use idyll_runtime::{Leased, Read};
use num_traits::FromPrimitive as _;
use protocol::{Dir, Recipient, RequestTypeType, StdRequestCode};
use userlib::{RecvMessage, ReplyFaultReason};
use drv_stm32l4_sys_api::{Stm32L4Sys, Port, Function};
use stm32_metapac::usb::vals::{EpType, Stat};

#[export_name = "main"]
fn main() -> ! {
    let sys = Stm32L4Sys::from(hubris_task_slots::SLOTS.sys);
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

    let mut server = Server {
        usb,
        pending_address: None,
        state: DeviceState::Powered,
    };
    userlib::sys_enable_irq(hubris_notifications::USB_IRQ);

    let mut incoming = [MaybeUninit::uninit(); USB_HID_BUFFER_SIZE];
    loop {
        idyll_runtime::dispatch_or_event(&mut server, hubris_notifications::USB_IRQ, &mut incoming);
    }
}

struct Server {
    usb: stm32_metapac::usb::Usb,
    pending_address: Option<u8>,
    state: DeviceState,
}

#[derive(Copy, Clone, Debug, Default)]
enum DeviceState {
    #[default]
    Powered,
    Default,
    Address,
    Configured,
}

#[no_mangle]
static OUR_ADDRESS: AtomicU8 = AtomicU8::new(0);

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
        OUR_ADDRESS.store(0, Ordering::Relaxed);
        self.pending_address = None;
        self.state = DeviceState::Default;
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
                            emit(Event::AddressPending(addr));
                            self.pending_address = Some(addr);
                            usbsram::set_ep_tx_count(ep, 0);
                            self.configure_response(ep, Stat::VALID, Stat::VALID);
                        }
                        (Dir::DeviceToHost, Some(StdRequestCode::GetDescriptor)) => {
                            let txoff = usbsram::get_ep_tx_offset(ep);
                            emit(Event::GetDescriptor(setup.value.get()));
                            match protocol::prepare_descriptor(&setup, txoff) {
                                Some(len) => {
                                    emit(Event::DescriptorReady(len));
                                    usbsram::set_ep_tx_count(ep, setup.length.get().min(len as u16));
                                    self.configure_response(ep, Stat::VALID, Stat::VALID);
                                }
                                None => {
                                    emit(Event::UnknownDescriptor);
                                    usbsram::set_ep_tx_count(ep, 0);
                                    self.configure_response(ep, Stat::STALL, Stat::STALL);
                                }
                            }
                        }
                        (Dir::HostToDevice, Some(StdRequestCode::SetConfiguration)) => {
                            usbsram::set_ep_tx_count(ep, 0);
                            //self.iface.on_set_config(usb); TODO
                            self.configure_response(ep, Stat::VALID, Stat::VALID);
                            self.state = DeviceState::Configured;
                        }
                        (x, y) => {
                            emit(Event::UnknownStandard(x, y));
                            usbsram::set_ep_tx_count(ep, 0);
                            self.configure_response(ep, Stat::STALL, Stat::STALL);
                        }
                    }
                }
                //(RequestTypeType::Standard, Recipient::Interface) => {
                //    // TODO
                //}
                //(RequestTypeType::Class, Recipient::Interface) => {
                //    // TODO
                //}
                (x, y) => {
                    emit(Event::UnknownTop(x, y));
                    usbsram::set_ep_tx_count(ep, 0);
                    self.configure_response(ep, Stat::STALL, Stat::STALL);
                }
            }
        }
    }

    fn on_out(&mut self, ep: usize) {
        emit(Event::Out(ep));
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
                OUR_ADDRESS.store(addr, Ordering::Relaxed);
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
}

#[no_mangle]
static IRQS: AtomicUsize = AtomicUsize::new(0);

fn leave_toggles_unchanged(v: &mut stm32_metapac::usb::regs::Epr) {
    v.set_dtog_tx(false);
    v.set_dtog_rx(false);
    v.set_stat_tx(Stat::from_bits(0));
    v.set_stat_rx(Stat::from_bits(0));
}

impl idyll_runtime::NotificationHandler for Server {
    fn handle_notification(&mut self, bits: u32) {
        if bits & hubris_notifications::USB_IRQ != 0 {
            IRQS.fetch_add(1, Ordering::Relaxed);

            let istr = self.usb.istr().read();
            emit(Event::Istr(istr));

            // Detect and handle link reset.
            if istr.reset() {
                emit(Event::Reset);
                self.device_reset();
                userlib::sys_enable_irq(hubris_notifications::USB_IRQ);
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
                emit(Event::Epr(eprv));

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

            userlib::sys_enable_irq(hubris_notifications::USB_IRQ);
        }
    }
}

impl UsbHid for Server {
    fn enqueue_report(
        &mut self,
        _full_msg: &RecvMessage<'_>,
        _endpoint: u8,
        _data: Leased<Read, u8>,
    ) -> Result<Result<bool, EnqueueError>, ReplyFaultReason> {
        Err(ReplyFaultReason::UndefinedOperation)
    }
}

include!(concat!(env!("OUT_DIR"), "/generated_server.rs"));

#[no_mangle]
static mut EVENTS: [Event; 128] = [Event::Reset; 128];
#[no_mangle]
static NEXT_EVENT: AtomicUsize = AtomicUsize::new(0);

#[derive(Copy, Clone)]
enum Event {
    Reset,
    Setup(usize),
    Out(usize),
    In(usize),
    AddressPending(u8),
    DescriptorReady(usize),
    UnknownDescriptor,
    Address(u8),
    Istr(stm32_metapac::usb::regs::Istr),
    Epr(stm32_metapac::usb::regs::Epr),
    UnknownStandard(Dir, Option<StdRequestCode>),
    UnknownTop(RequestTypeType, Recipient),
    GetDescriptor(u16),
}

fn emit(event: Event) {
    let i = NEXT_EVENT.load(Ordering::Relaxed);
    let buf = unsafe { &mut *addr_of_mut!(EVENTS) };
    if let Some(slot) = buf.get_mut(i) {
        *slot = event;
        NEXT_EVENT.store(i + 1, Ordering::Relaxed);
    }
}
