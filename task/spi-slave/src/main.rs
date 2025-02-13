//! A task that controls a SPI device.
#![no_std]
#![no_main]

use drv_stm32xx_sys_api::{Function::AF0, PeripheralName, Port, Stm32Sys as Sys};
use hubris_task_slots::SLOTS;
use stm32_metapac::spi::vals::{
    Cpha::FIRST_EDGE, Cpol::IDLE_LOW, Ds::BITS8, Frxth::QUARTER, Mstr::SLAVE,
};

// SPI1 (mode_0, slave):
// PB0 NSS (O, AF0)
// PB3 SCK (O, AF0)
// PB4 MISO (I, AF0)
// PB5 MOSI (O, AF0)

#[export_name = "main"]
fn main() -> ! {
    // Create a client for the Sys driver
    let sys = Sys::from(SLOTS.sys);

    let spi_rcv = SLOTS.spircv;
    let spi_op: u16 = 1;

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
        // w.set_ssm(false);
        w.set_ssm(true);
        w.set_ssi(false);
        w.set_spe(true);
    });

    sys.set_pin_alternate_mode(Port::B, 0, AF0); // NSS
    sys.set_pin_alternate_mode(Port::B, 3, AF0); // SCK
    sys.set_pin_alternate_mode(Port::B, 4, AF0); // MISO
    sys.set_pin_alternate_mode(Port::B, 5, AF0); // MOSI

    userlib::sys_enable_irq(hubris_notifications::SPI_IRQ);

    let mut data: [u8; 10] = [0; 10];

    let mut incoming = [];

    let mut leases = [];

    loop {
        let mut i = 0;

        userlib::sys_enable_irq_and_clear_pending(hubris_notifications::SPI_IRQ);
        userlib::sys_recv_notification(hubris_notifications::SPI_IRQ);

        // read all pending characters
        while spi.sr().read().rxne() {
            data[i] = spi.dr8().read();

            i += 1;
        }

        let mut l: [u8; 1] = [0; 1];
        l[0] = i as u8;
        let _r = userlib::sys_send(spi_rcv, spi_op, &l, &mut incoming, &mut leases);
    }
}
