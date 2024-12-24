#![no_std]
#![no_main]

use stm32_metapac as pac;
use stm32_metapac::{pwr::vals::Vos, rcc::vals::{Plldiv, Pllm, Pllmboost, Plln, Pllrge, Pllsrc, Sw}};
use cortex_m_rt::entry;

fn clock_setup() -> u32 {
    if cfg!(feature = "clock-160mhz-hsi16") {
        // Turn on the HSI16 clock source.
        pac::RCC.cr().modify(|w| w.set_hsion(true));
        while !pac::RCC.cr().read().hsion() {
            // spin
        }

        // Configure PLL1 to use HSI16 as its input clock source, and set up the
        // EPOD booster divider. Interestingly, ST says to do this _before_
        // configuring the PLL, because it generates a clock that's used behind
        // the scenes.
        pac::RCC.pll1cfgr().modify(|w| {
            w.set_pllmboost(Pllmboost::DIV1);
            w.set_pllsrc(Pllsrc::HSI);
        });

        pac::RCC.ahb3enr().modify(|w| w.set_pwren(true));
        cortex_m::asm::dsb();

        // Transition to power Range 1 and enable the EPOD boost, which is
        // evidently required at speeds over 55 MHz.
        pac::PWR.vosr().modify(|w| {
            w.set_vos(Vos::RANGE1);
            w.set_boosten(true);
        });

        // Wait until the VOS switch completes.
        while !pac::PWR.vosr().read().vosrdy() {
            // spin
        }

        // Wait until the booster is live.
        while !pac::PWR.vosr().read().boostrdy() {
            // spin
        }

        // The ICACHE appears to be pretty damn important on this part, when
        // wait states are enabled. There does not appear to be a separate flash
        // accelerator, so without the ICACHE, all we get is
        // next-sequential-line prefetching. In testing this causes a delay loop
        // that leaves its flash line to take dramatically longer than one which
        // is line-aligned.
        //
        // Fortunately turning on the ICACHE appears to be pretty easy. A cache
        // invalidation operation is kicked off at reset; wait for it to confirm
        // completion:
        while !pac::ICACHE.sr().read().bsyendf() {
            // spin
        }
        // And then turn it on:
        pac::ICACHE.cr().modify(|w| w.set_en(true));

        // Add a wait state to flash accesses to prepare for 160 MHz.
        pac::FLASH.acr().modify(|w| w.set_latency(4));
        while pac::FLASH.acr().read().latency() != 4 {
            // spin
        }

        // Enable the flash prefetcher, since we have wait states now. This is
        // not as critical as it was on ART accelerator parts, but is described
        // as speeding up cache fills.
        pac::FLASH.acr().modify(|w| w.set_prften(true));

        // Configure PLL1. It's getting 16 MHz from HSI16; we're gonna do the
        // following math:
        pac::RCC.pll1cfgr().modify(|w| {
            w.set_pllfracen(false);
            // Input frequency (16 MHz) is already in range, we don't have to
            // divide the input.
            w.set_pllm(Pllm::DIV1);
            w.set_pllrge(Pllrge::FREQ_8TO16MHZ);
            // Enable the R tap.
            w.set_pllren(true);
        });
        pac::RCC.pll1divr().modify(|w| {
            // Multiply reference clock by 16 to produce a VCO frequency of
            // 160 MHz.
            w.set_plln(Plln::MUL10);
            // Don't divide that on the R tap, so we get 160 MHz out.
            w.set_pllr(Plldiv::DIV1);
        });

        // Turn it on and wait!
        pac::RCC.cr().modify(|w| {
            w.set_pllon(0, true);
        });
        while pac::RCC.cr().read().pllrdy(0) {
            // spin
        }

        // Switch the system clock over to PLL1R.
        pac::RCC.cfgr1().modify(|w| w.set_sw(Sw::PLL1_R));
        while pac::RCC.cfgr1().read().sws() != Sw::PLL1_R {
            // spin
        }

        // Alright, we should now be at 160 MHz.
        160_000
    } else {
        // Nothing to see here
        4_000
    }
}

#[entry]
fn main() -> ! {
    let cycles_per_ms = clock_setup();

    unsafe { hubris_kern::startup::start_kernel(cycles_per_ms) }
}
