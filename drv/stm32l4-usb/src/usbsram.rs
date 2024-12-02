//! USB SRAM access and layout.
//!
//! Currently I'm not using the linker to manage USB SRAM. Instead, I'm treating
//! it as a fixed layout, which is as follows:
//!
//! ```text
//! 000 buffer descriptor table
//! 040 tx buffer 0
//! 080 rx buffer 0
//! 0c0 tx buffer 1
//! 100 rx buffer 1
//! 140 end of allocated space
//! ```

use core::ptr::{read_volatile, write_volatile};

use zerocopy::{FromBytes, Immutable, IntoBytes};
use zerocopy_derive::{FromBytes, Immutable, IntoBytes};

#[derive(FromBytes, IntoBytes, Immutable)]
#[repr(C)]
struct BufferDescriptor {
    txaddr: u16,
    txcount: u16,
    rxaddr: u16,
    rxcount: u16,
}

pub fn fill_buffer_descriptor_table(usb: &stm32_metapac::usb::Usb) {
    // Initially clear the buffer descriptor table, which conveniently also
    // indicates to the hardware that no buffers are available.
    for i in (0..0x40).step_by(2) {
        write16(i, 0);
    }

    // Initialize the buffers to a known value to make debugging, etc. easier.
    for i in (0x40..0x140).step_by(2) {
        write16(i, 0xDEAD);
    }

    // Override the first two buffer descriptor table entries for our expected
    // two endpoints.
    for i in 0..2 {
        let rx_bytes = 0x40;
        let (bl_size, num_block) = if rx_bytes < 62 {
            debug_assert!(rx_bytes >= 2);
            (0, rx_bytes as u16 >> 1)
        } else {
            (1, rx_bytes as u16 / 32 - 1)
        };
        write(size_of::<BufferDescriptor>() * i, &BufferDescriptor {
            txaddr: u16::try_from(get_ep_tx_offset(i)).unwrap(),
            txcount: 0,
            rxaddr: u16::try_from(get_ep_rx_offset(i)).unwrap(),
            rxcount: (num_block << 10) | (bl_size << 15),
        });
    }

    // And load the table address into the peripheral.
    usb.btable().write(|w| w.set_btable(0));
}

pub fn get_ep_rx_offset(ep: usize) -> usize {
    0x80 + ep * 0x80
}

pub fn get_ep_tx_offset(ep: usize) -> usize {
    0x40 + ep * 0x80
}

pub fn set_ep_tx_count(ep: usize, count: u16) {
    write16(2 + 8 * ep, count);
}

const SRAM_BASE: usize = 0x4000_6c00;
const SRAM_SIZE: usize = 1024;

pub fn read16(addr: usize) -> u16 {
    debug_assert!(addr < SRAM_SIZE - 1);

    unsafe {
        read_volatile((SRAM_BASE + addr) as *const u16)
    }
}

pub fn write16(addr: usize, value: u16) {
    debug_assert!(addr < SRAM_SIZE - 1);

    unsafe {
        write_volatile((SRAM_BASE + addr) as *mut u16, value)
    }
}

fn read8(addr: usize) -> u8 {
    debug_assert!(addr < SRAM_SIZE);

    unsafe {
        read_volatile((SRAM_BASE + addr) as *const u8)
    }
}

pub fn write8(addr: usize, value: u8) {
    debug_assert!(addr < SRAM_SIZE);

    // This uses a read-modify-write after determining that an 8-bit write
    // affects the byte next to it. siiiiigh. Only 8-bit _reads_ appear to be
    // safe.
    let word_addr = addr & !1;

    let v = read16(word_addr);
    write16(word_addr, if addr & 1 == 0 {
        (v & 0xFF00) | u16::from(value)
    } else {
        (v & 0xFF) | u16::from(value) << 8
    });
}

pub fn write_bytes(mut addr: usize, mut data: &[u8]) {
    debug_assert!(addr < SRAM_SIZE);
    debug_assert!(addr + data.len() <= SRAM_SIZE);

    // Handle misaligned initial byte.
    if addr & 1 != 0 {
        let Some((byte, rest)) = data.split_first() else {
            // Empty input? Sure, why not.
            return;
        };
        write8(addr, *byte);
        addr += 1;
        data = rest;
    }

    // Write as much as possible in 16-bit chunks.
    for d in data.chunks(2) {
        if let Ok(pair) = <[u8; 2]>::try_from(d) {
            let halfword = u16::from_le_bytes(pair);
            write16(addr, halfword);
            addr += 2;
        } else {
            // One-byte final chunk.
            write8(addr, d[0]);
            addr += 1; // should be final, but, hey
        }
    }
}

pub fn write<T>(addr: usize, value: &T)
    where T: IntoBytes + Immutable,
{
    write_bytes(addr, value.as_bytes());
}

pub fn read_bytes(mut addr: usize, mut data: &mut [u8]) {
    debug_assert!(addr < SRAM_SIZE);
    debug_assert!(addr + data.len() <= SRAM_SIZE);

    // Handle misaligned initial byte.
    if addr & 1 != 0 {
        let Some((byte, rest)) = data.split_first_mut() else {
            // Empty input? Sure, why not.
            return;
        };
        *byte = read8(addr);
        addr += 1;
        data = rest;
    }

    // Read as much as possible in 16-bit chunks.
    for d in data.chunks_mut(2) {
        if let Ok(pair) = <&mut [u8; 2]>::try_from(&mut *d) {
            *pair = read16(addr).to_le_bytes();
            addr += 2;
        } else {
            // One-byte final chunk.
            d[0] = read8(addr);
            addr += 1; // should be final, but, hey
        }
    }
}

pub fn read<T>(addr: usize) -> T
    where T: Sized + FromBytes + IntoBytes,
{
    let mut value = T::new_zeroed();
    read_bytes(addr, value.as_mut_bytes());
    value
}

