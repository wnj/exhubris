//! STM32G0 "sys" implementation

#![no_std]
#![no_main]

use core::mem::MaybeUninit;

use stm32_metapac::gpio::vals::{Moder, Odr};
use userlib::{sys_recv_open, sys_reply, sys_reply_fault, RecvMessage, ReplyFaultReason, ResponseCode};

#[export_name = "main"]
fn main() -> ! {
    let mut incoming = [MaybeUninit::uninit(); STM_32_G_0_SYS_BUFFER_SIZE];
    let rcc = stm32_metapac::RCC;

    // Turn on clock to the GPIO ports.
    rcc.gpioenr().modify(|r| r.0 |= 0b101111);

    loop {
        idyll_runtime::dispatch(&mut Server, &mut incoming);
    }
}

fn get_port(index: Port) -> stm32_metapac::gpio::Gpio {
    match index {
        Port::A => stm32_metapac::GPIOA,
        Port::B => stm32_metapac::GPIOB,
        Port::C => stm32_metapac::GPIOC,
        Port::D => stm32_metapac::GPIOD,
        Port::F => stm32_metapac::GPIOF,
    }
}

fn convert_pin(pin: u8) -> usize {
    usize::from(pin) & 0xF
}

struct Server;

impl Stm32G0Sys for Server {
    fn set_pin_output(&mut self, _: &RecvMessage<'_>, port:Port, pin:u8) -> Result<(),idyll_runtime::ReplyFaultReason> {
        let gpio = get_port(port);
        gpio.moder().modify(|v| v.set_moder(convert_pin(pin), Moder::OUTPUT));
        Ok(())
    }

    fn set_pin_high(&mut self, _: &RecvMessage<'_>, port:Port, pin:u8) -> Result<(),idyll_runtime::ReplyFaultReason> {
        let gpio = get_port(port);
        gpio.bsrr().write(|v| {
            v.set_bs(convert_pin(pin), true);
        });
        Ok(())
    }

    fn set_pin_low(&mut self, _: &RecvMessage<'_>, port:Port, pin:u8) -> Result<(),idyll_runtime::ReplyFaultReason> {
        let gpio = get_port(port);
        gpio.bsrr().write(|v| {
            v.set_br(convert_pin(pin), true);
        });
        Ok(())
    }

    fn toggle_pin(&mut self, _: &RecvMessage<'_>, port:Port, pin:u8) -> Result<(),idyll_runtime::ReplyFaultReason> {
        let gpio = get_port(port);
        let pin = convert_pin(pin);
        let state = gpio.odr().read().odr(pin) == Odr::HIGH;
        gpio.bsrr().write(|v| {
            if state {
                v.set_br(pin, true);
            } else {
                v.set_bs(pin, true);
            }
        });

        Ok(())
    }

    fn set_pin_alternate_mode(&mut self, _: &RecvMessage<'_>, port:Port, pin:u8, function:Function) -> Result<(),idyll_runtime::ReplyFaultReason> {
        let pin = convert_pin(pin);
        let gpio = get_port(port);
        let af = function as u8;
        gpio.afr(if pin < 8 { 0 } else { 1 }).modify(|r| r.set_afr(pin & 0b111, af));
        gpio.moder().modify(|v| v.set_moder(pin, Moder::ALTERNATE));

        Ok(())
    }

    fn enable_clock(&mut self, _: &RecvMessage<'_>, peripheral:PeripheralName) -> Result<(),idyll_runtime::ReplyFaultReason> {
        let bits = peripheral as u16;
        let bit_no = usize::from(bits & 0x1F);
        let reg_no = bits >> 5;
        let rcc = stm32_metapac::RCC;
        match reg_no {
            0 => rcc.gpioenr().modify(|r| r.0 |= 1 << bit_no),
            1 => rcc.ahbenr().modify(|r| r.0 |= 1 << bit_no),
            2 => rcc.apbenr1().modify(|r| r.0 |= 1 << bit_no),
            _ => rcc.apbenr2().modify(|r| r.0 |= 1 << bit_no),
        }

        Ok(())
    }
}

include!(concat!(env!("OUT_DIR"), "/generated_server.rs"));
