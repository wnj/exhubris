//! A task that controls a SPI device.
#![no_std]
#![no_main]

use drv_stm32xx_sys_api::{
    Function::{AF0, AF1},
    PeripheralName, Port, Stm32Sys as Sys,
};
use hubris_task_slots::SLOTS;
use stm32_metapac::spi::vals::{
    Br::DIV64, Cpha::FIRST_EDGE, Cpol::IDLE_LOW, Ds::BITS8, Frxth::QUARTER, Mstr::MASTER,
};
use stm32_metapac::spi::Spi;
use stm32_metapac::usart::Usart;

// SPI1 (mode_0, 1 MHz):
// PB0 NSS (O, AF0)
// PB3 SCK (O, AF0)
// PB4 MISO (I, AF0)
// PB5 MOSI (O, AF0)

// nRF24L01 register dump:
// 0x00 = 0x08
// 0x01 = 0x3f
// 0x02 = 0x03
// 0x03 = 0x03
// 0x04 = 0x03
// 0x05 = 0x02
// 0x06 = 0x0f
// 0x07 = 0x0e
// 0x08 = 0x00
// 0x09 = 0x00
// 0x0a = 0xe7 0xe7 0xe7 0xe7 0xe7
// 0x0b = 0xc2 0xc2 0xc2 0xc2 0xc2
// 0x0c = 0xc3
// 0x0d = 0xc4
// 0x0e = 0xc5
// 0x0f = 0xc6
// 0x10 = 0xe7 0xe7 0xe7 0xe7 0xe7
// 0x11 = 0x00
// 0x12 = 0x00
// 0x13 = 0x00
// 0x14 = 0x00
// 0x15 = 0x00
// 0x16 = 0x00
// 0x17 = 0x11
// 0x18 = 0x00
// 0x19 = 0x00
// 0x1a = 0x00
// 0x1b = 0x00
// 0x1c = 0x00

#[derive(Clone, Copy)]
pub struct Abc {}

impl core::fmt::Display for Abc {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "fmt...")
    }
}

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
        // w.set_rxneie(true);
        w.set_re(true);
        w.set_te(true);
        w.set_ue(true);
    });

    // Set pin A2+A3 to USART
    sys.set_pin_alternate_mode(Port::A, 2, AF1);
    sys.set_pin_alternate_mode(Port::A, 3, AF1);

    write_string("Hello nRF24L01\r\n".as_bytes(), uart);

    // turn on SPI1
    sys.enable_clock(PeripheralName::Spi1);

    // initialize SPI
    let spi = stm32_metapac::SPI1;

    sys.set_pin_output(Port::B, 0); // NSS
    sys.set_pin_high(Port::B, 0);

    spi.cr2().write(|w| w.set_ssoe(false));

    spi.cr2().write(|w| {
        w.set_frxth(QUARTER);
        w.set_ds(BITS8);
    });
    spi.cr1().write(|w| {
        w.set_cpha(FIRST_EDGE);
        w.set_cpol(IDLE_LOW);
        w.set_br(DIV64);
        w.set_mstr(MASTER);
        w.set_ssm(true);
        w.set_ssi(true);
        w.set_spe(true);
    });

    sys.set_pin_alternate_mode(Port::B, 3, AF0); // SCK
    sys.set_pin_alternate_mode(Port::B, 4, AF0); // MISO
    sys.set_pin_alternate_mode(Port::B, 5, AF0); // MOSI

    let mut buf: [u8; 6] = [0; 6];

    // for reg in 0..0x1d {
    for reg in 0..0x1d {
        buf[0] = 0b0000_0000 | reg;

        let mut hex: [u8; 33] = [' ' as u8; 33];

        to_hex2(buf[0], &mut hex[0..4]); //cmd

        hex[5] = '=' as u8;

        if reg == 0xa || reg == 0xb || reg == 0x10 {
            transfer(&mut buf[0..6], spi);
            to_hex2(buf[1], &mut hex[7..11]);
            to_hex2(buf[2], &mut hex[12..16]);
            to_hex2(buf[3], &mut hex[17..21]);
            to_hex2(buf[4], &mut hex[22..26]);
            to_hex2(buf[5], &mut hex[27..31]);
            hex[31] = '\r' as u8;
            hex[32] = '\n' as u8;
            write_string(&hex, uart);
        } else {
            transfer(&mut buf[0..2], spi);
            to_hex2(buf[1], &mut hex[7..11]);
            hex[11] = '\r' as u8;
            hex[12] = '\n' as u8;
            write_string(&hex[0..13], uart);
        }
    }

    // Record the current time so we can start our delay loop properly.
    let mut next_send = userlib::sys_get_timer().now;

    const INTERVAL: u64 = 3; // milliseconds

    loop {
        userlib::sys_set_timer(Some(next_send), hubris_notifications::TIMER_SPI);

        // The proper thing to do, when waiting for a timer, is to sleep waiting
        // for notifications _and then check the time._ Otherwise other tasks
        // can wake you up by posting.
        loop {
            userlib::sys_recv_notification(hubris_notifications::TIMER_SPI);
            let now = userlib::sys_get_timer().now;
            if now >= next_send {
                next_send += INTERVAL;
                break;
            }
        }
    }
}

// transfer bytes to spi device
fn transfer(data: &mut [u8], spi: Spi) {
    let sys = Sys::from(SLOTS.sys);

    sys.set_pin_low(Port::B, 0);

    if spi.sr().read().txe() {
        for k in 0..data.len() {
            spi.dr8().write_value(data[k]);
            cortex_m::asm::delay(400); // slow down, we read too fast!
            data[k] = spi.dr8().read();
        }
    }

    sys.set_pin_high(Port::B, 0);
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
    hex[0] = '0' as u8;
    hex[1] = 'x' as u8;
    hex[2] = HEX_CHARS[(word / 16) as usize];
    hex[3] = HEX_CHARS[(word % 16) as usize];
}
