//! STM32xx generic "sys" implementation
//!
//! This crate is intended to provide a driver handling RCC+GPIO on all STM32
//! variants. In practice, it needs a bit of code added to support each new
//! family.

#![no_std]
#![no_main]

use core::mem::MaybeUninit;

use cfg_if::cfg_if;
use stm32_metapac::gpio::vals::{Idr, Moder, Odr, Pupdr};
use idyll_runtime::Meta;
use userlib::ReplyFaultReason;

#[export_name = "main"]
fn main() -> ! {
    let mut incoming = [MaybeUninit::uninit(); STM_32_SYS_BUFFER_SIZE];
    let rcc = stm32_metapac::RCC;

    // Turn on clock to the GPIO ports.
    cfg_if! {
        if #[cfg(hubris_chip = "STM32L412")] {
            rcc.ahb2enr().modify(|r| r.0 |= 0b1000_1111);
        } else if #[cfg(hubris_chip = "STM32G031")] {
            rcc.gpioenr().modify(|r| r.0 |= 0b101111);
        } else {
            compile_error!("unimplemented target chip");
        }
    }

    loop {
        idyll_runtime::dispatch(&mut Server, &mut incoming);
    }
}

fn convert_pin(pin: u8) -> usize {
    usize::from(pin) & 0xF
}

fn screen_af(af: Function) -> Result<u8, ReplyFaultReason> {
    let af = af as u8;
    if cfg!(hubris_chip = "STM32G0") {
        // Parts that only support AF0-7
        if af > 7 {
            return Err(ReplyFaultReason::BadMessageContents);
        }
    }

    Ok(af)
}

struct Server;

impl Stm32Sys for Server {
    fn set_pin_output(&mut self, _: Meta, port:Port, pin:u8) -> Result<(),ReplyFaultReason> {
        let gpio = get_port(port)?;
        gpio.moder().modify(|v| v.set_moder(convert_pin(pin), Moder::OUTPUT));
        Ok(())
    }

    fn set_pin_high(&mut self, _: Meta, port:Port, pin:u8) -> Result<(),ReplyFaultReason> {
        let gpio = get_port(port)?;
        gpio.bsrr().write(|v| {
            v.set_bs(convert_pin(pin), true);
        });
        Ok(())
    }

    fn set_pin_low(&mut self, _: Meta, port:Port, pin:u8) -> Result<(),ReplyFaultReason> {
        let gpio = get_port(port)?;
        gpio.bsrr().write(|v| {
            v.set_br(convert_pin(pin), true);
        });
        Ok(())
    }

    fn toggle_pin(&mut self, _: Meta, port:Port, pin:u8) -> Result<(),ReplyFaultReason> {
        let gpio = get_port(port)?;
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

    fn set_pin_alternate_mode(&mut self, _: Meta, port:Port, pin:u8, function:Function) -> Result<(),ReplyFaultReason> {
        let pin = convert_pin(pin);
        let gpio = get_port(port)?;
        let af = screen_af(function)?;
        gpio.afr(if pin < 8 { 0 } else { 1 }).modify(|r| r.set_afr(pin & 0b111, af));
        gpio.moder().modify(|v| v.set_moder(pin, Moder::ALTERNATE));

        Ok(())
    }

    fn set_pin_input(&mut self, _: Meta, port:Port, pin:u8) -> Result<(),ReplyFaultReason> {
        let gpio = get_port(port)?;
        gpio.moder().modify(|v| v.set_moder(convert_pin(pin), Moder::INPUT));
        Ok(())
    }

    fn is_pin_high(&mut self, _: Meta, port:Port, pin:u8) -> Result<bool,ReplyFaultReason> {
        let gpio = get_port(port)?;
        let idr = gpio.idr().read();
        Ok(idr.idr(usize::from(pin & 0xF)) == Idr::HIGH)
    }

    fn set_pin_pull(&mut self, _: Meta, port:Port, pin:u8, pull: Option<Pull>) -> Result<(),ReplyFaultReason> {
        let gpio = get_port(port)?;
        gpio.pupdr().modify(|v| {
            v.set_pupdr(convert_pin(pin), match pull {
                Some(Pull::Up) => Pupdr::PULLUP,
                Some(Pull::Down) => Pupdr::PULLDOWN,
                None => Pupdr::FLOATING,
            });
        });
        gpio.moder().modify(|v| v.set_moder(convert_pin(pin), Moder::INPUT));
        Ok(())
    }

    fn enable_clock(&mut self, _: Meta, peripheral:PeripheralName) -> Result<(),ReplyFaultReason> {
        let bits = peripheral as u16;
        let bit_no = usize::from(bits & 0x1F);
        let reg_no = bits >> 5;
        let rcc = stm32_metapac::RCC;

        cfg_if! {
            if #[cfg(hubris_chip = "STM32L412")] {
                match reg_no {
                    0 => rcc.ahb1enr().modify(|r| r.0 |= 1 << bit_no),
                    1 => rcc.ahb2enr().modify(|r| r.0 |= 1 << bit_no),
                    2 => rcc.ahb3enr().modify(|r| r.0 |= 1 << bit_no),
                    3 => rcc.apb1enr1().modify(|r| r.0 |= 1 << bit_no),
                    4 => rcc.apb1enr2().modify(|r| r.0 |= 1 << bit_no),
                    _ => rcc.apb2enr().modify(|r| r.0 |= 1 << bit_no),
                }
            } else if #[cfg(hubris_chip = "STM32G031")] {
                match reg_no {
                    0 => rcc.gpioenr().modify(|r| r.0 |= 1 << bit_no),
                    1 => rcc.ahbenr().modify(|r| r.0 |= 1 << bit_no),
                    2 => rcc.apbenr1().modify(|r| r.0 |= 1 << bit_no),
                    _ => rcc.apbenr2().modify(|r| r.0 |= 1 << bit_no),
                }
            } else {
                compile_error!("unsupported chip family");
            }
        }

        Ok(())
    }

    fn set_pins_output(
        &mut self,
        _: Meta,
        port: Port,
        mask: u16,
    ) -> Result<(),ReplyFaultReason> {
        let gpio = get_port(port)?;
        gpio.moder().modify(|w| {
            for i in 0..16 {
                if mask & (1 << i) == 0 {
                    continue;
                }
                w.set_moder(i, Moder::OUTPUT);
            }
        });
        Ok(())
    }

    fn read_pins(
        &mut self,
        _: Meta,
        port: Port,
    ) -> Result<u16,ReplyFaultReason> {
        let gpio = get_port(port)?;
        Ok(gpio.idr().read().0 as u16)
    }
}

/// Returns the GPIO port corresponding to `port`, if implemented on this chip.
/// If not implemented, returns a fault.
fn get_port(port: Port) -> Result<stm32_metapac::gpio::Gpio, ReplyFaultReason> {
    match port {
        Port::A => Ok(stm32_metapac::GPIOA),
        Port::B => Ok(stm32_metapac::GPIOB),
        Port::C => Ok(stm32_metapac::GPIOC),
        Port::D => Ok(stm32_metapac::GPIOD),

        #[cfg(hubris_chip = "STM32G031")]
        Port::F => Ok(stm32_metapac::GPIOF),

        #[cfg(hubris_chip = "STM32L412")]
        Port::H => Ok(stm32_metapac::GPIOH),

        _ => Err(ReplyFaultReason::BadMessageContents),
    }
}

include!(concat!(env!("OUT_DIR"), "/generated_server.rs"));
