//! STM32G0 "sys" implementation

#![no_std]
#![no_main]

use core::mem::MaybeUninit;

use stm32_metapac::gpio::vals::{Moder, Odr};
use userlib::{sys_recv_open, sys_reply, sys_reply_fault, RecvMessage, ReplyFaultReason, ResponseCode};

#[export_name = "main"]
fn main() -> ! {
    let mut incoming = [MaybeUninit::uninit(); 32];
    let rcc = stm32_metapac::RCC;

    // Turn on clock to the GPIO ports.
    rcc.gpioenr().modify(|r| r.0 |= 0b101111);

    loop {
        let rm = sys_recv_open(&mut incoming, 0);
        let sender = rm.sender;
        match handle_message(rm) {
            Ok(()) => sys_reply(sender, ResponseCode::SUCCESS, &[]),
            Err(f) => sys_reply_fault(sender, f),
        }
    }
}

fn handle_message(rm: RecvMessage<'_>) -> Result<(), ReplyFaultReason> {
    // Note: we do not check for TaskId::KERNEL because our mask above is 0.
    let Ok(data) = rm.data else {
        return Err(ReplyFaultReason::BadMessageSize);
    };
    match rm.operation_or_notification {
        0 => {
            // Set a port/pin to output
            let Ok(msg) = <&[u8; 2]>::try_from(&*data) else {
                return Err(ReplyFaultReason::BadMessageSize);
            };
            let msg = u16::from_le_bytes(*msg);
            let pin = usize::from(msg & 0xF);
            let gpio = get_port(msg >> 4)?;
            gpio.moder().modify(|v| v.set_moder(pin, Moder::OUTPUT));

            Ok(())
        }
        1 | 2 => {
            // Set a port/pin to high/low
            let Ok(msg) = <&[u8; 2]>::try_from(&*data) else {
                return Err(ReplyFaultReason::BadMessageSize);
            };
            let msg = u16::from_le_bytes(*msg);
            let pin = usize::from(msg & 0xF);
            let gpio = get_port(msg >> 4)?;
            gpio.bsrr().write(|v| {
                v.set_bs(pin, rm.operation_or_notification == 1);
            });

            Ok(())
        }
        3 => {
            // Toggle a port/pin
            let Ok(msg) = <&[u8; 2]>::try_from(&*data) else {
                return Err(ReplyFaultReason::BadMessageSize);
            };
            let msg = u16::from_le_bytes(*msg);
            let pin = usize::from(msg & 0xF);
            let gpio = get_port(msg >> 4)?;
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
        _ => Err(ReplyFaultReason::UndefinedOperation),
    }
}

fn get_port(index: u16) -> Result<stm32_metapac::gpio::Gpio, ReplyFaultReason> {
    match index {
        0 => Ok(stm32_metapac::GPIOA),
        1 => Ok(stm32_metapac::GPIOB),
        2 => Ok(stm32_metapac::GPIOC),
        3 => Ok(stm32_metapac::GPIOD),
        //4 => stm32_metapac::GPIOE,
        5 => Ok(stm32_metapac::GPIOF),
        _ => Err(ReplyFaultReason::BadMessageContents),
    }
}
