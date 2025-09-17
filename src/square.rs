// ! Square type: 0..=63 indexing where A1=0, H1=7, A8=56, H8=63.
// ! From White's perspective: North=+8, South=-8, East=+1, West=-1.
// ! This newtype wraps a `u8` to avoid mixing raw integers with board squares.
#![allow(dead_code)]

use std::fmt::{self, Formatter, Display};

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Square(u8);

impl Square {
  /// Create a square from a 0..=63 index. Returns `None` if out of range.
  pub fn from_index(i: u8) -> Option<Square> {
    if i >63 {
      None
    }
    else {
      Some(Square(i))
    }
  }

  /// Create a square from file and rank (0..7). Returns `None` if out of range.
  pub fn from_file_rank(file: u8, rank: u8) -> Option<Square> {
    if file > 7 || rank > 7 {
      None
    }
    else {
      Some(Square(rank * 8 + file))
    }
  }

  /// Return the underlying 0..=63 index.
  pub fn index(&self) -> u8 { self.0 }


  /// Get the file (0..7) of the square.
  pub fn file(&self) -> u8 {
    self.0 % 8
  }

  /// Get the rank (0..7) of the square.
  pub fn rank(&self) -> u8 {
    self.0 / 8
  }

  /// Offset the square by a signed delta. Returns `None` if out of range.
  pub fn offset(&self, delta: i8) -> Option<Square> {
    let new_index = self.0 as i8 + delta;
    if !(0..=63).contains(&new_index) {
      None
    }
    else {
      Some(Square(new_index as u8))
    }
  }
}

impl Display for Square {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let file_char = (b'a' + self.file()) as char; // algebraic uses lowercase a..h
        let rank_char = (b'1' + self.rank()) as char; // '1'..'8'
        write!(f, "{}{}", file_char, rank_char)
    }
}




# [cfg(test)]
mod tests {
    use super::Square;

    #[test]
    fn from_index_valid() {
        assert!(Square::from_index(0).is_some()); // a1
        assert!(Square::from_index(7).is_some()); // h1
        assert!(Square::from_index(56).is_some()); // a8
        assert!(Square::from_index(63).is_some()); // h8
    }

    #[test]
    fn from_index_invalid() {
        assert!(Square::from_index(64).is_none());
    }

    #[test]
    fn file_rank_roundtrip() {
        let a1 = Square::from_index(0).unwrap();
        assert_eq!(a1.file(), 0);
        assert_eq!(a1.rank(), 0);
        let h1 = Square::from_index(7).unwrap();
        assert_eq!(h1.file(), 7);
        assert_eq!(h1.rank(), 0);
        let a8 = Square::from_index(56).unwrap();
        assert_eq!(a8.file(), 0);
        assert_eq!(a8.rank(), 7);
        let h8 = Square::from_index(63).unwrap();
        assert_eq!(h8.file(), 7);
        assert_eq!(h8.rank(), 7);
    }

    #[test]
    fn offset_bounds() {
        let a1 = Square::from_index(0).unwrap();
        assert!(a1.offset(-1).is_none()); // west off-board
        assert_eq!(a1.offset(8).unwrap().index(), 8); // north to a2
        let h8 = Square::from_index(63).unwrap();
        assert!(h8.offset(1).is_none()); // east off-board
    }

    #[test]
    fn display_algebraic() {
        let a1 = Square::from_index(0).unwrap();
        assert_eq!(a1.to_string(), "a1");
        let h8 = Square::from_index(63).unwrap();
        assert_eq!(h8.to_string(), "h8");
    }
}