//! A task that controls a SPI device.
#![no_std]
#![no_main]

use drv_stm32xx_sys_api::{
    Function::{AF0, AF1},
    PeripheralName, Port, Stm32Sys as Sys,
};
use hubris_task_slots::SLOTS;
use stm32_metapac::spi::vals::{
    Cpha::FIRST_EDGE, Cpol::IDLE_LOW, Ds::BITS8, Frxth::QUARTER, Mstr::SLAVE,
};
use stm32_metapac::usart::Usart;

// SPI1 (mode_0, slave):
// PB0 NSS (O, AF0)
// PB3 SCK (O, AF0)
// PB4 MISO (I, AF0)
// PB5 MOSI (O, AF0)

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
        w.set_re(true);
        w.set_te(true);
        w.set_ue(true);
    });

    // Set pin A2+A3 to USART
    sys.set_pin_alternate_mode(Port::A, 2, AF1);
    sys.set_pin_alternate_mode(Port::A, 3, AF1);

    write_string("Hello SPI slave\r\n".as_bytes(), uart);

    // turn on SPI1
    sys.enable_clock(PeripheralName::Spi1); //???

    // initialize SPI
    let spi = stm32_metapac::SPI1;

    spi.cr2().write(|w| w.set_ssoe(false));

    spi.cr2().write(|w| {
        w.set_frxth(QUARTER);
        w.set_ds(BITS8);
        w.set_rxneie(true);
    });
    spi.cr1().write(|w| {
        w.set_cpha(FIRST_EDGE);
        w.set_cpol(IDLE_LOW);
        w.set_mstr(SLAVE);
        w.set_spe(true);
    });

    // sys.set_pin_alternate_mode(Port::B, 0, AF0); // NSS
    sys.set_pin_alternate_mode(Port::B, 3, AF0); // SCK
    sys.set_pin_alternate_mode(Port::B, 4, AF0); // MISO
    sys.set_pin_alternate_mode(Port::B, 5, AF0); // MOSI

    userlib::sys_enable_irq(hubris_notifications::SPI_IRQ);

    let mut data: [u8; 10] = [0; 10];

    loop {
        let mut i = 0;
        let mut buf: [u8; 3] = [' ' as u8; 3];

        userlib::sys_enable_irq_and_clear_pending(hubris_notifications::SPI_IRQ);
        userlib::sys_recv_notification(hubris_notifications::SPI_IRQ);

        // read all pending characters
        while spi.sr().read().rxne() {
            data[i] = spi.dr8().read();

            i += 1;
        }

        for k in 0..i {
            to_hex2(data[k], &mut buf);
            write_string(&buf, uart);
        }

        // to_hex2(i as u8, &mut buf);
        // write_string(&buf, uart);
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
