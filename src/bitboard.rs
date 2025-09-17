//! Bitboard: a 64-bit mask where each bit corresponds to a board square (A1=bit 0 … H8=bit 63).
//! Squares follow the same indexing as `Square`: A1=0, H1=7, A8=56, H8=63.
//! Common operations: build sets of squares, test membership, count bits, and iterate via pop_lsb.
#![allow(dead_code)]
use crate::square::Square;

use std::{fmt::{self, Display, Formatter}, ops::{BitAnd, BitOr, Not}};

/// Newtype wrapper around `u64` so we don't mix raw integers with bitboards.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Bitboard(pub u64);

impl Bitboard {
  /// Create an empty bitboard (no bits set).
  pub fn empty() -> Self { Bitboard(0) }

  /// Returns true if any bit is set.
  pub fn any(self) -> bool { self.0 != 0 }

  /// Returns true if no bits are set.
  pub fn is_empty(self) -> bool { self.0 == 0 }

  /// Count of set bits (population count). Also called `popcnt` in engines.
  pub fn count(self) -> u32 { self.0.count_ones() }

  /// Index (0..=63) of the least-significant set bit, or None if empty.
  pub fn lsb(self) -> Option<u8> {
    if self.0 == 0 { None } else { Some(self.0.trailing_zeros() as u8) }
  }

  /// Pop the least-significant set bit: return its index and clear it.
  pub fn pop_lsb(&mut self) -> Option<u8> {
    if self.0 == 0 { return None; }
    let idx = self.0.trailing_zeros() as u8;
    // Clear that bit efficiently: x & (x - 1) drops the lowest set bit.
    self.0 &= self.0 - 1;
    Some(idx)
  }

  /// Set the bit corresponding to `sq`.
  pub fn set(&mut self, sq: Square) { self.0 |= 1u64 << sq.index(); }

  /// Test if the bit for `sq` is set.
  pub fn test(self, sq: Square) -> bool { (self.0 & (1u64 << sq.index())) != 0 }
}

// Utility function
impl BitOr for Bitboard {
  type Output = Self;

  fn bitor(self, rhs: Self) -> Self::Output {
    Bitboard(self.0 | rhs.0)
  }
}

impl BitAnd for Bitboard {
  type Output = Self;

  fn bitand(self, rhs: Self) -> Self::Output {
    Bitboard(self.0 & rhs.0)
  }
}

impl Not for Bitboard {
  type Output = Self;

  fn not(self) -> Self::Output {
    Bitboard(!self.0)
  }
}


// Displays the bitboard as an 8x8 grid with rank/file labels.
// Top row is rank 8, bottom row is rank 1; files run a..h left to right.
impl Display for Bitboard {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    // Print ranks 8..1 from top to bottom
    for rank in (0..8).rev() {
      // Rank labels on the left
      write!(f, "{} ", rank + 1)?;
      for file in 0..8 {
        let sq = Square::from_file_rank(file, rank).unwrap();
        let ch = if self.test(sq) { '1' } else { '0' };
        // Space-separated for easier reading
        write!(f, "{} ", ch)?;
      }
      writeln!(f)?;
    }
    // File labels at the bottom
    write!(f, "  a b c d e f g h")
  }
}

#[cfg(test)]
mod tests {
    use super::Bitboard;
    use crate::square::Square;

    #[test]
    fn empty_any_is_empty() {
        let bb = Bitboard::empty();
        assert!(bb.is_empty());
        assert!(!bb.any());
        assert_eq!(bb.count(), 0);
        assert_eq!(bb.lsb(), None);
    }

    #[test]
    fn set_and_test_bits() {
        let mut bb = Bitboard::empty();
        let a1 = Square::from_index(0).unwrap();
        let h8 = Square::from_index(63).unwrap();
        bb.set(a1);
        bb.set(h8);
        assert!(bb.any());
        assert_eq!(bb.count(), 2);
        assert!(bb.test(a1));
        assert!(bb.test(h8));
        // a2 should be unset
        let a2 = Square::from_index(8).unwrap();
        assert!(!bb.test(a2));
    }

    #[test]
    fn lsb_and_pop_lsb_iteration() {
        let mut bb = Bitboard(0);
        let a1 = Square::from_index(0).unwrap();
        let b1 = Square::from_index(1).unwrap();
        let h8 = Square::from_index(63).unwrap();
        bb.set(h8);
        bb.set(a1);
        bb.set(b1);
        // lsb should be a1 (0)
        assert_eq!(bb.lsb(), Some(0));
        // pop sequence should be 0,1,63 then empty
        assert_eq!(bb.pop_lsb(), Some(0));
        assert_eq!(bb.pop_lsb(), Some(1));
        assert_eq!(bb.pop_lsb(), Some(63));
        assert_eq!(bb.pop_lsb(), None);
        assert!(bb.is_empty());
    }
}