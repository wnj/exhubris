use num_derive::FromPrimitive;
use num_traits::FromPrimitive as _;
use smart_default::SmartDefault;
use utf16_literal::utf16;
use zerocopy::{little_endian::U16, IntoBytes};
use zerocopy_derive::{FromBytes, IntoBytes, Immutable, Unaligned};

use crate::usbsram;

#[derive(Copy, Clone, Debug, Default, FromBytes, IntoBytes, Immutable, Unaligned)]
#[repr(C)]
pub struct SetupPacket {
    pub request_type: RequestType,
    pub request: u8,
    pub value: U16,
    pub index: U16,
    pub length: U16,
}

#[derive(Copy, Clone, Debug, Default, FromBytes, IntoBytes, Unaligned, Immutable)]
#[repr(transparent)]
pub struct RequestType(u8);

impl RequestType {
    pub fn data_phase_direction(self) -> Dir {
        if self.0 & 0x80 == 0 {
            Dir::HostToDevice
        } else {
            Dir::DeviceToHost
        }
    }

    pub fn type_(self) -> RequestTypeType {
        match (self.0 >> 5) & 0b11 {
            0 => RequestTypeType::Standard,
            1 => RequestTypeType::Class,
            2 => RequestTypeType::Vendor,
            _ => RequestTypeType::Reserved,
        }
    }

    pub fn recipient(self) -> Recipient {
        match self.0 & 0x1F {
            0 => Recipient::Device,
            1 => Recipient::Interface,
            2 => Recipient::Endpoint,
            3 => Recipient::Other,
            x => Recipient::Reserved(x),
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Dir {
    HostToDevice,
    DeviceToHost,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum RequestTypeType {
    Standard = 0,
    Class = 1,
    Vendor = 2,
    Reserved = 3,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Recipient {
    Device,
    Interface,
    Endpoint,
    Other,
    Reserved(u8),
}

#[derive(Copy, Clone, Debug, FromPrimitive, IntoBytes, Unaligned, Immutable)]
#[repr(u8)]
enum DescriptorType {
    Device = 1,
    Configuration = 2,
    String = 3,
    Interface = 4,
    Endpoint = 5,
}

#[derive(Copy, Clone, Debug, FromPrimitive)]
#[repr(u8)]
pub enum StdRequestCode {
    GetStatus = 0,
    ClearFeature = 1,
    SetFeature = 3,
    SetAddress = 5,
    GetDescriptor = 6,
    SetDescriptor = 7,
    GetConfiguration = 8,
    SetConfiguration = 9,
    GetInterface = 10,
    SetInterface = 11,
    SynchFrame = 12,
}

#[derive(Clone, Debug, IntoBytes, Unaligned, SmartDefault, Immutable)]
#[repr(C)]
struct DeviceDescriptor {
    #[default = 18]
    length: u8,
    #[default(DescriptorType::Device)]
    type_: DescriptorType,
    #[default(U16::new(0x0101))]
    usb_version: U16,
    device_class: u8,
    device_subclass: u8,
    device_protocol: u8,
    max_packet_size0: u8,
    vendor: U16,
    product: U16,
    device_version: U16,
    manufacturer_string: u8,
    product_string: u8,
    serial_string: u8,
    num_configurations: u8,
}

#[derive(Clone, Debug, IntoBytes, Unaligned, SmartDefault, Immutable)]
#[repr(C)]
struct ConfigDescriptor {
    #[default = 9]
    length: u8,
    #[default(DescriptorType::Configuration)]
    type_: DescriptorType,
    total_length: U16,
    num_interfaces: u8,
    configuration_value: u8,
    configuration_string: u8,
    attributes: u8,
    max_power: u8,
}


pub fn prepare_descriptor(
    setup: &SetupPacket,
    offset: usize,
) -> Option<usize> {
    let dtype = DescriptorType::from_u16(setup.value.get() >> 8)?;
    let idx = setup.value.get() as u8;

    let xxx = |desc| {
        usbsram::write_bytes(offset, desc);
        Some(desc.len())
    };

    match (dtype, idx) {
        (DescriptorType::Device, 0) => xxx(DeviceDescriptor {
            max_packet_size0: 64,
            vendor: U16::new(0xdead),
            product: U16::new(0xbeef),
            device_version: U16::new(0x3_14),
            manufacturer_string: 1,
            product_string: 2,
            serial_string: 3,
            num_configurations: 1,
            ..DeviceDescriptor::default()
        }.as_bytes()),
        (DescriptorType::Configuration, 0) => xxx(CompoundConfig {
            config: ConfigDescriptor {
                total_length: U16::new(core::mem::size_of::<CompoundConfig>() as u16),
                num_interfaces: 1,
                configuration_value: 1,
                configuration_string: 2,
                attributes: 0x80,
                max_power: 250,
                ..ConfigDescriptor::default()
            },
            iface: InterfaceDescriptor {
                interface_number: 0,
                alternate_setting: 0,
                num_endpoints: 1,
                interface_class: 3,
                interface_subclass: 1,
                interface_protocol: 1,
                interface_string: 2,
                ..InterfaceDescriptor::default()
            },
            hid: crate::hid::HidDescriptor {
                hid_version: U16::new(0x0101),
                country_code: 0,
                descriptor_type: 0x22,
                descriptor_length: U16::new(62),
                ..crate::hid::HidDescriptor::default()
            },
            ep: EndpointDescriptor {
                endpoint_address: 0x81,
                attributes: 0b11,
                max_packet_size: 8.into(),
                interval: 1,
                ..EndpointDescriptor::default()
            },
        }.as_bytes()),
        (DescriptorType::String, _) => {
            let s: &[u16] = match idx {
                0 => &[0x0409], // en_US
                1 => utf16!("🦶HUBRIS"),
                2 => utf16!("Test Device 🎉"),
                3 => utf16!("🦀"),
                _ => return None,
            };
            Some(write_str(offset, s))
        }
        _ => None,
    }
}

fn write_str(offset: usize, data: &[u16]) -> usize {
    let len = data.len() * 2 + 2;
    usbsram::write8(offset, len as u8);
    usbsram::write8(offset + 1, DescriptorType::String as u8);
    for (i, hw) in data.iter().enumerate() {
        usbsram::write16(offset + 2 + 2 * i, *hw);
    }
    len
}

#[derive(Clone, Debug, IntoBytes, Unaligned, Default, Immutable)]
#[repr(C)]
struct CompoundConfig {
    config: ConfigDescriptor,
    iface: InterfaceDescriptor,
    hid: crate::hid::HidDescriptor,
    ep: EndpointDescriptor,
}

#[derive(Clone, Debug, IntoBytes, Unaligned, SmartDefault, Immutable)]
#[repr(C)]
struct InterfaceDescriptor {
    #[default = 9]
    length: u8,
    #[default(DescriptorType::Interface)]
    type_: DescriptorType,
    interface_number: u8,
    alternate_setting: u8,
    num_endpoints: u8,
    interface_class: u8,
    interface_subclass: u8,
    interface_protocol: u8,
    interface_string: u8,
}

#[derive(Clone, Debug, IntoBytes, Unaligned, SmartDefault, Immutable)]
#[repr(C)]
struct EndpointDescriptor {
    #[default = 7]
    length: u8,
    #[default(DescriptorType::Endpoint)]
    type_: DescriptorType,
    endpoint_address: u8,
    attributes: u8,
    max_packet_size: U16,
    interval: u8,
}
