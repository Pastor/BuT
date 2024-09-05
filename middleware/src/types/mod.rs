mod bit;

use bit::BitSlice;
use std::fmt::{Debug, Display, Formatter};

#[derive(Default, Clone)]
enum Address {
    #[default]
    None,
    Default,
    Direct(u64),
    Offset(u64, u8),
}

impl Display for Address {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Address::None => f.write_str(""),
            Address::Default => f.write_str(""),
            Address::Direct(address) => f.write_fmt(format_args!("{:#X}", address)),
            Address::Offset(address, offset) =>
                f.write_fmt(format_args!("{:#X}:{}", address, offset))
        }
    }
}

#[derive(Default)]
enum Type {
    #[default]
    None,
    Bit,
    Alias(Box<Type>),
    AliasNamed(String),
    ArrayNamed(String, u64),
    Array(Box<Type>, u64),
}

trait TypeResolver {
    fn type_resolve(&self, type_name: &str) -> Type;
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::None => f.write_str("()"),
            Type::Bit => f.write_str("bit"),
            Type::Alias(bt) => f.write_fmt(format_args!("{:}", bt)),
            Type::AliasNamed(name) => f.write_str(name),
            Type::Array(bt, size) => f.write_fmt(format_args!("[{}: {:}]", size, bt)),
            Type::ArrayNamed(nam, size) => f.write_fmt(format_args!("[{}: {}]", size, nam)),
        }
    }
}

#[derive(Default)]
pub(crate) enum Variable {
    #[default]
    None,
    Address(Type, Address),
    Local(Type, BitSlice),
    #[deprecated]
    Unassigned(Type),
}

impl Variable {
    fn local(ty: Type) -> Self {
        Self::Local(ty, BitSlice::new())
    }

    fn unassigned(ty: Type) -> Self {
        Self::Unassigned(ty)
    }

    fn local_initialized(ty: Type, bits: &[u8]) -> Self {
        if let Type::ArrayNamed(_, size) = ty {
            if size != (bits.len() * 8) as u64 {
                return Variable::None;
            }
        } else if let Type::Array(_, size) = ty {
            if size != (bits.len() * 8) as u64 {
                return Variable::None;
            }
        }
        Self::Local(ty, BitSlice::from(bits))
    }

    fn address(ty: Type, address: Address) -> Self {
        if let Address::None = address.clone() {
            Self::None
        } else {
            Self::Address(ty, address)
        }
    }

    fn none() -> Self {
        Self::None
    }

    fn to_u8(&self) -> Result<u8, &str> {
        match self {
            Variable::None => Err("Can't convert to u8"),
            Variable::Address(typ, address) => Err("Can't fetch u8 on address"),
            Variable::Local(typ, data) => Ok(u8::from_be_bytes(data.bytes().as_slice().try_into().unwrap())),
            Variable::Unassigned(typ) => Err("Variable not assignment")
        }
    }

    fn to_u16(&self) -> Result<u16, &str> {
        match self {
            Variable::None => Err("Can't convert to u16"),
            Variable::Address(typ, address) => Err("Can't fetch u8 on address"),
            Variable::Local(typ, data) => Ok(u16::from_be_bytes(data.bytes().as_slice().try_into().unwrap())),
            Variable::Unassigned(typ) => Err("Variable not assignment")
        }
    }

    fn to_u32(&self) -> Result<u32, &str> {
        match self {
            Variable::None => Err("Can't convert to u32"),
            Variable::Address(typ, address) => Err("Can't fetch u8 on address"),
            Variable::Local(typ, data) => Ok(u32::from_be_bytes(data.bytes().as_slice().try_into().unwrap())),
            Variable::Unassigned(typ) => Err("Variable not assignment")
        }
    }

    fn to_u64(&self) -> Result<u64, &str> {
        match self {
            Variable::None => Err("Can't convert to u64"),
            Variable::Address(typ, address) => Err("Can't fetch u8 on address"),
            Variable::Local(typ, data) => Ok(u64::from_be_bytes(data.bytes().as_slice().try_into().unwrap())),
            Variable::Unassigned(typ) => Err("Variable not assignment")
        }
    }

    fn to_u128(&self) -> Result<u128, &str> {
        match self {
            Variable::None => Err("Can't convert to u128"),
            Variable::Address(typ, address) => Err("Can't fetch u8 on address"),
            Variable::Local(typ, data) => Ok(u128::from_be_bytes(data.bytes().as_slice().try_into().unwrap())),
            Variable::Unassigned(typ) => Err("Variable not assignment")
        }
    }
}

impl Display for Variable {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Variable::None => f.write_str(""),
            Variable::Address(typ, address) => f.write_fmt(format_args!("{:} = {:}", typ, address)),
            Variable::Local(typ, bits) => f.write_fmt(format_args!("{:} = {:?}", typ, bits.bytes())),
            Variable::Unassigned(typ) => f.write_fmt(format_args!("{:}", typ)),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::types::Type::{Array, Bit};
    use crate::types::Variable;

    #[test]
    fn type_defined() {
        let eight = Array(Box::new(Bit), 8);
        assert_eq!("[8: bit]", format!("{:}", eight));
    }

    #[test]
    fn variables() {
        let eight = Array(Box::new(Bit), 8);
        let var = Variable::local_initialized(eight, &[0, 0, 0, 0, 0, 1, 0, 1]);
        assert_eq!(5, var.to_u8().unwrap());
        assert_eq!(5, var.to_u16().unwrap());
        assert_eq!(5, var.to_u32().unwrap());
        assert_eq!(5, var.to_u64().unwrap());
        assert_eq!(5, var.to_u128().unwrap());
    }
}