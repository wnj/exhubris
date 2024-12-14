#![no_std]
#![no_main]

use stm32_metapac::{self as device, flash::vals::Latency, rcc::vals::{Pllm, Plln, Pllp, Pllq, Pllr, Pllsrc, Sw}};

use cortex_m_rt::entry;

#[entry]
fn main() -> ! {
    let ticks_per_ms = if cfg!(feature = "clock-64mhz-hsi16") {
        let flash = device::FLASH;
        let rcc = device::RCC;
        // We come out of reset at 16 MHz on HSI. We would like to be running at 64
        // MHz.
        //
        // This implies that we need to boost the clock speed by 4 using the PLL.
        //
        // Note: to achieve this, we must be in voltage range 1 (required for
        // frequencies above 16 MHz). The part appears to reset into voltage
        // range 1, so, that was easy.
        //
        // The PLL's input frequency can go up to 16 MHz, and its internal VCO must
        // be between 64 and 344 MHz (in voltage range 1). Its R-tap is the one that
        // feeds the CPU and buses, so we need the R output to be 64 MHz. The R
        // divisor is limited to integers between 2 and 8 (inclusive), so, we can
        // get what we want by
        //
        // - Setting the PLL input divisor (VCO multiplier) to 8x for an fVCO of
        //   128 MHz.
        // - Setting the R divisor to /2 for a 64 MHz output.
        // - Leaving the P and Q taps off.

        // Adjust our wait states to reflect our target voltage + freq combination.
        // At 64 MHz we'll need 2 wait states. We come out of reset at 0 wait
        // states, so we must override it.
        //
        // Note: the SVD apparently incorrectly models an undocumented reserved bit
        // in this register (bit 18), so DO NOT use `write` or `reset`. If you clear
        // bit 18, bad shit will happen to you -- it interferes with debugger
        // access.
        flash.acr().modify(|w| {
            w.set_prften(true);
            w.set_latency(Latency::WS2);
        });
        while flash.acr().read().latency() != Latency::WS2 {
            // spin
        }

        // Fire up the PLL.
        // First, set up our divisors.
        rcc.pllcfgr().modify(|w| {
            // Source input from HSI16.
            w.set_pllsrc(Pllsrc::HSI);
            // Do not divide the input.
            w.set_pllm(Pllm::DIV1);
            // Multiply that by 8 in the VCO for 128 MHz.
            w.set_plln(Plln::MUL8);
            // Configure the R-tap to 64 MHz output by dividing by 2. Configure P
            // and Q to valid configurations while we're at it.
            w.set_pllr(Pllr::DIV2);
            w.set_pllp(Pllp::DIV2);
            w.set_pllq(Pllq::DIV2);
            // But we only actually turn the R tap on.
            w.set_pllren(true);
        });
        // Switch it on at the RCC and wait for it to come up. RCC should still be
        // at its reset value (and doesn't have any weird undocumented bits), so
        // we don't need to RMW it.
        rcc.cr().write(|w| {
            w.set_pllon(true);
        });
        while !rcc.cr().read().pllrdy() {
            // spin
        }
        // Now switch over to it.
        rcc.cfgr().write(|w| {
            w.set_sw(Sw::PLL1_R);
        });
        while rcc.cfgr().read().sws() != Sw::PLL1_R {
            // spin
        }

        64_000
    } else {
        16_000
    };

    unsafe { hubris_kern::startup::start_kernel(ticks_per_ms) }
}
