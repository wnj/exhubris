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
