//! A task that blinks a LED at PC13.
//!
//! This task exists to test the build system.

#![no_std]
#![no_main]

use drv_stm32xx_sys_api::{Function::AF1, PeripheralName, Port, Stm32Sys as Sys};
use hubris_task_slots::SLOTS;
use stm32_metapac::usart::Usart;

use userlib::{sys_recv_msg_open, sys_reply, ResponseCode};

#[no_mangle]
static mut MESSAGE_COUNT: u32 = 0;

#[export_name = "main"]
fn main() -> ! {
    // Create a client for the Sys driver
    let sys = Sys::from(SLOTS.sys);

    // turn on USART2
    sys.enable_clock(PeripheralName::Usart2);

    // initialize UART
    let uart: stm32_metapac::usart::Usart = stm32_metapac::USART2;

    uart.brr().write(|w| {
        w.set_brr((64_000_000 / 115200) as u16);
    });

    uart.cr1().write(|w| {
        w.set_re(true);
        w.set_te(true);
        w.set_ue(true);
    });

    // Set pin A2+A3 to USART
    sys.set_pin_alternate_mode(Port::A, 2, AF1);
    sys.set_pin_alternate_mode(Port::A, 3, AF1);

    write_string("Hello SPI message logger\r\n".as_bytes(), uart);

    let mut buffer = [core::mem::MaybeUninit::uninit(); 32];

    loop {
        let rm = sys_recv_msg_open(&mut buffer);
        unsafe {
            MESSAGE_COUNT = MESSAGE_COUNT.wrapping_add(1);
        }

        sys_reply(rm.sender, ResponseCode::SUCCESS, &[]);

        let mut s: [u8; 2] = [0; 2];
        to_hex2(unsafe { buffer[0].assume_init_read() }, &mut s);
        write_string(&s, uart);
        write_string("\r\n".as_bytes(), uart);
    }
}

// write stream of characters terminated
fn write_string(s: &[u8], uart: Usart) {
    for k in 0..s.len() {
        write_ch(s[k], uart);
    }
}

// write single character
fn write_ch(ch: u8, uart: Usart) {
    uart.tdr().write(|w| w.set_dr(ch as u16));

    // wait till data is transmitted
    loop {
        if uart.isr().read().txe() {
            break;
        }
    }
}

pub const HEX_CHARS: &[u8; 16] = b"0123456789abcdef";

// fn to_hex2(word: u8, hex: &mut [u8]) {
fn to_hex2(word: u8, hex: &mut [u8]) {
    hex[0] = HEX_CHARS[(word / 16) as usize];
    hex[1] = HEX_CHARS[(word % 16) as usize];
}
