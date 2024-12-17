#![no_std]
#![no_main]

use stm32l4::stm32l412 as device;

use cortex_m_rt::entry;

cfg_if::cfg_if! {
    if #[cfg(feature = "clock-80mhz-hsi16")] {
        fn clock_setup(p: &mut device::Peripherals) {

            // Scale the CPU up to our max frequency of 80MHz.
            //
            // We come out of reset at 4MHz on MSI. We'd like to move over to
            // HSI16 for greater accuracy, and then raise that to 80 with the
            // PLL.
            //
            // HSI16 is already in a suitable range for the PLL, but could be
            // further divided if it enables us to improve (reduce) VCO
            // frequency. However, the CPU runs on the PLL's R-tap, and the
            // R-tap has very few divisor options. This means our VCO frequency
            // is the constrained part. The easiest choice without using
            // fractional mode is a div-by-4 from a VCO frequency of 320MHz --
            // div-by-2 and div-by-6 would each put the frequency out of range.
            // (This part does not have an adjustable VCO input frequency
            // range.)
            //
            // 16MHz input * 20 = 320MHz VCO
            // 320MHz VCO / 4 = 80MHz clock
            //
            // That gives us DIVN1=20, DIVP1=3.
            //
            // We'll turn off the Q and R taps for now.

            // Getting to 80MHz requires the CPU to be volted at its highest setting of
            // VOS1. It _appears_ to come out of reset at VOS1. So that was easy.

            // Turn on HSI16.
            p.RCC.cr.modify(|_, w| w.hsion().set_bit());
            while !p.RCC.cr.read().hsirdy().bit() {}

            // Configure PLL1.
            p.RCC.pllcfgr.write(|w| unsafe {
                w.pllpdiv().bits(0)
                    .pllr().bits(0b01)
                    .pllren().set_bit()
                    .pllq().bits(0b11)
                    .pllqen().clear_bit()
                    .pllp().clear_bit()
                    .pllpen().clear_bit()
                    .plln().bits(20)
                    .pllm().bits(0)
                    .pllsrc().bits(0b10)
            });
            // Switch on PLL1.
            p.RCC.cr.modify(|_, w| w.pllon().set_bit());
            while !p.RCC.cr.read().pllrdy().bit() {}

            // Adjust Flash wait states for target frequency.
            p.FLASH.acr.modify(|_, w| unsafe { w.latency().bits(0b100) });
            while p.FLASH.acr.read().latency() != 0b100 {}

            // Adjust bus clock dividers for target frequency - no changes should be
            // needed.

            // Switch CPU frequency.
            p.RCC.cfgr.modify(|_, w| unsafe { w.sw().bits(0b11) });
            while p.RCC.cfgr.read().sws() != 0b11 {}

            // Enable flash prefetching to compensate for wait states.
            p.FLASH.acr.modify(|_, w| w.prften().set_bit());

            // k cool

        }
        const CYCLES_PER_MS: u32 = 80_000;
    } else {
        fn clock_setup(p: &mut device::Peripherals) {
            // Nothing to see here
        }
        const CYCLES_PER_MS: u32 = 4_000;
    }
}

#[entry]
fn main() -> ! {
    let mut p = unsafe { device::Peripherals::steal() };
    clock_setup(&mut p);

    if cfg!(feature = "clock-hsi48-on") {
        // Turn on HSI48.
        p.RCC.crrcr.modify(|_, w| {
            w.hsi48on().set_bit()
        });
        while !p.RCC.crrcr.read().hsi48rdy().bit() {
            // spin
        }
        // Use HSI48 as the 48MHz reference.
        p.RCC.ccipr.modify(|_, w| w.clk48sel().bits(0b00));
    }

    if cfg!(feature = "pwr-vddusb-valid") {
        // Enable clock to the PWR unit so we can talk to it.
        p.RCC.apb1enr1.modify(|_, w| w.pwren().set_bit());
        cortex_m::asm::dsb();
        // Tell PWR that we expect VDDUSB to be available and valid.
        p.PWR.cr2.modify(|_, w| w.usv().set_bit());
    }

    if cfg!(feature = "kernel-profiling") {
        // Set up PA0 and PA1 to pulse on kernel events.
        p.RCC.ahb2enr.modify(|_, w| w.gpioaen().set_bit());
        cortex_m::asm::dsb();
        p.GPIOA.moder.modify(|_, w| {
            w.moder0().output()
                .moder1().output()
        });
        static EVENTS_TABLE: hubris_kern::profiling::EventsTable = hubris_kern::profiling::EventsTable {
            syscall_enter: |n| {
                let gpioa = unsafe { &*device::GPIOA::ptr() };
                gpioa.bsrr.write(|w| {
                    w.bs0().set_bit();
                    if n & 4 == 0 {
                        w.br1().set_bit();
                    } else {
                        w.bs1().set_bit();
                    }
                    w
                });
            },
            syscall_exit: || {
                let gpioa = unsafe { &*device::GPIOA::ptr() };
                gpioa.bsrr.write(|w| w.br0().set_bit().br1().set_bit());
            },
            secondary_syscall_enter: || {
            },
            secondary_syscall_exit: || {
            },
            isr_enter: || {
            },
            isr_exit: || {
            },
            timer_isr_enter: || (),
            timer_isr_exit: || (),
            context_switch: |_| {
            },
        };
        hubris_kern::profiling::configure_events_table(&EVENTS_TABLE);
    }

    unsafe { hubris_kern::startup::start_kernel(CYCLES_PER_MS) }
}
