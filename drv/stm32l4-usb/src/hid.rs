use num_derive::FromPrimitive;
use smart_default::SmartDefault;
use zerocopy::little_endian::U16;
use zerocopy_derive::{Immutable, IntoBytes, Unaligned};

#[derive(Copy, Clone, Debug, FromPrimitive, IntoBytes, Unaligned, Immutable)]
#[repr(u8)]
pub enum HidClassDescriptorType {
    Hid = 0x21,
    Report = 0x22,
    Physical = 0x33,
}

#[derive(Copy, Clone, Debug, FromPrimitive)]
#[repr(u8)]
pub enum HidRequestCode {
    GetReport = 1,
    GetIdle = 2,
    GetProtocol = 3,
    SetReport = 9,
    SetIdle = 0xA,
    SetProtocol = 0xB,
}

#[derive(Clone, Debug, IntoBytes, Unaligned, Immutable, SmartDefault)]
#[repr(C)]
pub struct HidDescriptor {
    #[default = 9]
    pub length: u8,
    #[default(HidClassDescriptorType::Hid)]
    pub type_: HidClassDescriptorType,
    pub hid_version: U16,
    pub country_code: u8,
    #[default = 1]
    pub num_descriptors: u8,
    pub descriptor_type: u8,
    pub descriptor_length: U16,
}

#[derive(Copy, Clone, Debug, Default)]
pub enum OutKind {
    #[default]
    SetReport
}

pub static BOOT_KBD_DESC: [u8; 62] = [
    0x05, 0x01,       //  Usage Page (Desktop),
    0x09, 0x06,       //  Usage (Keyboard),
    0xA1, 0x01,       //  Collection (Application),
    0x05, 0x07,       //      Usage Page (Keyboard),
    0x19, 0xE0,       //      Usage Minimum (KB Leftcontrol),
    0x29, 0xE7,       //      Usage Maximum (KB Right GUI),
    0x15, 0x00,       //      Logical Minimum (0),
    0x25, 0x01,       //      Logical Maximum (1),
    0x75, 0x01,       //      Report Size (1),
    0x95, 0x08,       //      Report Count (8),
    0x81, 0x02,       //      Input (Variable),
    0x95, 0x01,       //      Report Count (1),
    0x75, 0x08,       //      Report Size (8),
    0x81, 0x01,       //      Input (Constant),
    0x95, 0x03,       //      Report Count (3),
    0x75, 0x01,       //      Report Size (1),
    0x05, 0x08,       //      Usage Page (LED),
    0x19, 0x01,       //      Usage Minimum (01h),
    0x29, 0x03,       //      Usage Maximum (03h),
    0x91, 0x02,       //      Output (Variable),
    0x95, 0x05,       //      Report Count (5),
    0x75, 0x01,       //      Report Size (1),
    0x91, 0x01,       //      Output (Constant),
    0x95, 0x06,       //      Report Count (6),
    0x75, 0x08,       //      Report Size (8),
    0x26, 0xFF, 0x00, //      Logical Maximum (255),
    0x05, 0x07,       //      Usage Page (Keyboard),
    0x19, 0x00,       //      Usage Minimum (None),
    0x29, 0x91,       //      Usage Maximum (KB LANG2),
    0x81, 0x00,       //      Input,
    0xC0              //  End Collection
];
