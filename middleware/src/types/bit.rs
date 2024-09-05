use bit_set::BitSet;
use bit_vec::BitVec;
pub(crate) type Bit = (u128, bool);
pub struct BitSlice {
    inner: BitVec,
}

impl BitSlice {
    pub fn new() -> Self {
        Self { inner: BitVec::new() }
    }

    fn from_elem(n: usize, bit: bool) -> Self {
        Self {
            inner: BitVec::from_elem(n, bit)
        }
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn bytes(&self) -> Vec<u8> {
        self.inner.to_bytes()
    }

    fn create_set(&self) -> BitSet {
        BitSet::from_bit_vec(self.inner.clone())
    }

    pub fn from(bites: &[u8]) -> Self {
        Self {
            inner: BitVec::from_bytes(bites),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::BitSlice;

    #[test]
    fn test_bit_in() {
        let bits = BitSlice::from_elem(512, true);
        assert_eq!(512, bits.len());
        let bytes = bits.bytes();
        assert_eq!(64, bytes.len());
        let mut set = bits.create_set();
        set.get_mut().set(0, false);
        assert_eq!(false, set.contains(0));
        let n = u128::from_be_bytes(bytes.as_slice().try_into().unwrap());
        assert_ne!(n, 0);
    }
}
