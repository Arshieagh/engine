//! Precomputed-style attack generators for non-sliding pieces.
//! We use bitboard shifts with **file-edge masks** to prevent horizontal wrap.
//! Indexing matches `Square`: A1=0..H8=63; N=+8, S=-8, E=+1, W=-1.
#![allow(dead_code)]
#![allow(clippy::needless_range_loop)]

use crate::square::Square;
use crate::bitboard::{Bitboard};
use crate::{Color, color_idx};

use once_cell::sync::Lazy;

const FILE_A: u64 = 0x0101010101010101;  // a-file
const FILE_B: u64 = 0x0202020202020202;  // b-file
const FILE_G: u64 = 0x4040404040404040;  // g-file
const FILE_H: u64 = 0x8080808080808080;  // h-file

const FILE_AB: u64 = FILE_A | FILE_B; // a,b files
const FILE_GH: u64 = FILE_G | FILE_H; // g,h files

/// Knight attack mask on an empty board from `sq` (no blockers).
pub fn knight_attacks(sq: Square) -> Bitboard {
  let mut bitboard = Bitboard::empty();
  // Piece position
  let s = 1u64 << sq.index();

  bitboard.0 |= 
    (s & !FILE_A)  << 15 |   // 2N+1W
    (s & !FILE_H)  << 17 |   // 2N+1E
    (s & !FILE_GH) << 10 |   // 2E+1N
    (s & !FILE_GH) >> 6  |   // 2E+1S
    (s & !FILE_H)  >> 15 |   // 2S+1E
    (s & !FILE_A)  >> 17 |   // 2S+1W
    (s & !FILE_AB) >> 10 |  // 2W+1S
    (s & !FILE_AB) << 6;    // 2W+1N
  bitboard
}

/// Table-backed knight attacks (precomputed via once_cell::Lazy).
pub fn knight_attacks_tbl(sq: Square) -> Bitboard {
  KNIGHT_TBL[sq.index() as usize]
}

/// King attack mask on an empty board from `sq` (no blockers).
pub fn king_attacks(sq: Square) -> Bitboard {
  let mut bitboard = Bitboard::empty();
  // Piece position
  let s = 1u64 << sq.index();

  bitboard.0 |=
    (s & !FILE_A) << 7 | // NW
    (s & !FILE_A) >> 1 | // W
    (s & !FILE_A) >> 9 | // SW
    (s & !FILE_H) << 9 | // NE
    (s & !FILE_H) << 1 | // E
    (s & !FILE_H) >> 7 | // SE
    s << 8 | // N
    s >> 8;  // S
  bitboard
}

/// Table-backed king attacks (precomputed).
pub fn king_attacks_tbl(sq: Square) -> Bitboard {
  KING_TBL[sq.index() as usize]
}

/// Pawn attack mask on an empty board from `sq` for `color` (diagonals only; no pushes).
pub fn pawn_attacks(sq: Square, color: Color) -> Bitboard {
  let mut bitboard = Bitboard::empty();
  // Piece position
  let s = 1u64 << sq.index();

  match color {
    Color::White => {
      bitboard.0 |=
        (s & !FILE_A) << 7 | // NW
        (s & !FILE_H) << 9;  // NE
    }
    Color::Black => {
      bitboard.0 |=
        (s & !FILE_A) >> 9 | // SW
        (s & !FILE_H) >> 7;  // SE
    }
  }
  bitboard
}

/// Table-backed pawn attacks (precomputed).
pub fn pawn_attacks_tbl(sq: Square, color: Color) -> Bitboard {
  PAWN_TBL[color_idx(color)][sq.index() as usize]
}

/// Empty-board ray in the N direction from `sq` (stops at board edge).
pub fn ray_n(sq: Square) -> Bitboard {
  let mut bitboard = Bitboard::empty();
  let mut s = 1u64 << sq.index();

  while s != 0 {
    bitboard.0 |= s << 8;
    s <<= 8;
  }
  bitboard
}

/// Empty-board ray in the S direction from `sq` (stops at board edge).
pub fn ray_s(sq: Square) -> Bitboard {
  let mut bitboard = Bitboard::empty();
  let mut s = 1u64 << sq.index();

  while s != 0 {
    bitboard.0 |= s >> 8;
    s >>= 8;
  }
  bitboard
}

/// Empty-board ray in the E direction from `sq` (stops at board edge).
pub fn ray_e(sq: Square) -> Bitboard {
  let mut bitboard = Bitboard::empty();
  let mut s = 1u64 << sq.index();

  while (s & !FILE_H) != 0 {
    bitboard.0 |= s << 1;
    s <<= 1;
  }
  bitboard
}

/// Empty-board ray in the W direction from `sq` (stops at board edge).
pub fn ray_w(sq: Square) -> Bitboard {
  let mut bitboard = Bitboard::empty();
  let mut s = 1u64 << sq.index();

  while (s & !FILE_A) != 0 {
    bitboard.0 |= s >> 1;
    s >>= 1;
  }
  bitboard
}

/// Empty-board ray in the NE direction from `sq` (stops at board edge).
pub fn ray_ne(sq: Square) -> Bitboard {
  let mut bitboard = Bitboard::empty();
  let mut s = 1u64 << sq.index();

  while (s & !FILE_H) != 0 {
    bitboard.0 |= s << 9;
    s <<= 9;
  }
  bitboard
}

/// Empty-board ray in the NW direction from `sq` (stops at board edge).
pub fn ray_nw(sq: Square) -> Bitboard {
  let mut bitboard = Bitboard::empty();
  let mut s = 1u64 << sq.index();

  while (s & !FILE_A) != 0 {
    bitboard.0 |= s << 7;
    s <<= 7;
  }
  bitboard
}

/// Empty-board ray in the SE direction from `sq` (stops at board edge).
pub fn ray_se(sq: Square) -> Bitboard {
  let mut bitboard = Bitboard::empty();
  let mut s = 1u64 << sq.index();

  while (s & !FILE_H) != 0 {
    bitboard.0 |= s >> 7;
    s >>= 7;
  }
  bitboard
}

/// Empty-board ray in the SW direction from `sq` (stops at board edge).
pub fn ray_sw(sq: Square) -> Bitboard {
  let mut bitboard = Bitboard::empty();
  let mut s = 1u64 << sq.index();

  while (s & !FILE_A) != 0 {
    bitboard.0 |= s >> 9;
    s >>= 9;
  }
  bitboard
}

/// Blocker-aware attacks in the N direction: includes the first blocker and stops.
pub fn attack_n(occ: Bitboard, sq: Square) -> Bitboard {
  let mut bitboard = Bitboard::empty();
  let mut s = 1u64 << sq.index();

  while s != 0 {
    s <<= 8;
    bitboard.0 |= s;

    if (s & occ.0) != 0 { break; }
  }
  bitboard
}

/// Blocker-aware attacks in the S direction: includes the first blocker and stops.
pub fn attack_s(occ: Bitboard, sq: Square) -> Bitboard {
  let mut bitboard = Bitboard::empty();
  let mut s = 1u64 << sq.index();

  while s != 0 {
    s >>= 8;
    bitboard.0 |= s;

    if (s & occ.0) != 0 { break; }
  }
  bitboard
}

/// Blocker-aware attacks in the E direction: includes the first blocker and stops.
pub fn attack_e(occ: Bitboard, sq: Square) -> Bitboard {
  let mut bitboard = Bitboard::empty();
  let mut s = 1u64 << sq.index();

  while (s & !FILE_H) != 0 {
    s <<= 1;
    bitboard.0 |= s;

    if (s & occ.0) != 0 { break; }
  }
  bitboard
}

/// Blocker-aware attacks in the W direction: includes the first blocker and stops.
pub fn attack_w(occ: Bitboard, sq: Square) -> Bitboard {
  let mut bitboard = Bitboard::empty();
  let mut s = 1u64 << sq.index();

  while (s & !FILE_A) != 0 {
    s >>= 1;
    bitboard.0 |= s;

    if (s & occ.0) != 0 { break; }
  }
  bitboard
}

/// Blocker-aware attacks in the NE direction: includes the first blocker and stops.
pub fn attack_ne(occ: Bitboard, sq: Square) -> Bitboard {
  let mut bitboard = Bitboard::empty();
  let mut s = 1u64 << sq.index();

  while (s & !FILE_H) != 0 {
    s <<= 9;
    bitboard.0 |= s;

    if (s & occ.0) != 0 { break; }
  }
  bitboard
}

/// Blocker-aware attacks in the SE direction: includes the first blocker and stops.
pub fn attack_se(occ: Bitboard, sq: Square) -> Bitboard {
  let mut bitboard = Bitboard::empty();
  let mut s = 1u64 << sq.index();

  while (s & !FILE_H) != 0 {
    s >>= 7;
    bitboard.0 |= s;

    if (s & occ.0) != 0 { break; }
  }
  bitboard
}

/// Blocker-aware attacks in the NW direction: includes the first blocker and stops.
pub fn attack_nw(occ: Bitboard, sq: Square) -> Bitboard {
  let mut bitboard = Bitboard::empty();
  let mut s = 1u64 << sq.index();

  while (s & !FILE_A) != 0 {
    s <<= 7;
    bitboard.0 |= s;

    if (s & occ.0) != 0 { break; }
  }
  bitboard
}

/// Blocker-aware attacks in the SW direction: includes the first blocker and stops.
pub fn attack_sw(occ: Bitboard, sq: Square) -> Bitboard {
  let mut bitboard = Bitboard::empty();
  let mut s = 1u64 << sq.index();

  while (s & !FILE_A) != 0 {
    s >>= 9;
    bitboard.0 |= s;

    if (s & occ.0) != 0 { break; }
  }
  bitboard
}

/// Empty-board sliding attacks (no blockers).
pub fn rook_attacks_empty(sq: Square) -> Bitboard { ray_n(sq) | ray_s(sq) | ray_e(sq) | ray_w(sq) }
/// Empty-board sliding attacks (no blockers).
pub fn bishop_attacks_empty(sq: Square) -> Bitboard { ray_ne(sq) | ray_nw(sq) | ray_se(sq) | ray_sw(sq) }
/// Empty-board sliding attacks (no blockers).
pub fn queen_attacks_empty(sq: Square) -> Bitboard { rook_attacks_empty(sq) | bishop_attacks_empty(sq) }

/// Blocker-aware sliding attacks given an occupancy bitboard (includes first blocker in each ray).
pub fn rook_attacks(occ: Bitboard, sq: Square) -> Bitboard {
  attack_n(occ, sq) | attack_s(occ, sq) | attack_e(occ, sq) | attack_w(occ, sq)
}

/// Blocker-aware sliding attacks given an occupancy bitboard (includes first blocker in each ray).
pub fn bishop_attacks(occ: Bitboard, sq: Square) -> Bitboard {
  attack_ne(occ, sq) | attack_nw(occ, sq) | attack_se(occ, sq) | attack_sw(occ, sq)
}

/// Blocker-aware sliding attacks given an occupancy bitboard (includes first blocker in each ray).
pub fn queen_attacks(occ: Bitboard, sq: Square) -> Bitboard {
  rook_attacks(occ, sq) | bishop_attacks(occ, sq)
}

static KNIGHT_TBL: Lazy<[Bitboard; 64]> = Lazy::new(|| {
  let mut table = [Bitboard::empty(); 64];
  for s in 0..64 {
    table[s] = knight_attacks(Square::from_index(s as u8).unwrap());
  }
  table
});

static KING_TBL: Lazy<[Bitboard; 64]> = Lazy::new(|| {
  let mut table = [Bitboard::empty(); 64];
  for s in 0..64 {
    table[s] = king_attacks(Square::from_index(s as u8).unwrap());
  }
  table
});

static PAWN_TBL: Lazy<[[Bitboard; 64]; 2]> = Lazy::new(|| {
  let mut table = [[Bitboard::empty(); 64]; 2];
  for s in 0..64 {
    let sq = Square::from_index(s as u8).unwrap();
    table[0][s] = pawn_attacks(sq, Color::White);
    table[1][s] = pawn_attacks(sq, Color::Black);
  }
  table
});

#[cfg(test)]
mod tests {
    use super::*;
    use crate::square::Square;
    use crate::bitboard::Bitboard;

    fn bb_from_indices(idxs: &[u8]) -> Bitboard {
        let mut bb = Bitboard::empty();
        for &i in idxs { bb.0 |= 1u64 << i; }
        bb
    }

    #[test]
    fn knight_attacks_edges_and_center() {
        let a1 = Square::from_index(0).unwrap();
        let h8 = Square::from_index(63).unwrap();
        let d4 = Square::from_index(27).unwrap();
        assert_eq!(knight_attacks(a1).count(), 2);   // b3,c2
        assert_eq!(knight_attacks(h8).count(), 2);   // f7,g6
        assert_eq!(knight_attacks(d4).count(), 8);
    }

    #[test]
    fn king_attacks_edges_and_center() {
        let a1 = Square::from_index(0).unwrap();
        let h8 = Square::from_index(63).unwrap();
        let d4 = Square::from_index(27).unwrap();
        assert_eq!(king_attacks(a1).count(), 3);     // a2,b1,b2
        assert_eq!(king_attacks(h8).count(), 3);     // g8,g7,h7
        assert_eq!(king_attacks(d4).count(), 8);
    }

    #[test]
    fn pawn_attacks_white_black_edges() {
        let b2 = Square::from_index(9).unwrap();
        let a2 = Square::from_index(8).unwrap();
        let h2 = Square::from_index(15).unwrap();
        let b7 = Square::from_index(49).unwrap();
        let a7 = Square::from_index(48).unwrap();
        let h7 = Square::from_index(55).unwrap();
        assert_eq!(pawn_attacks(b2, Color::White).count(), 2); // a3,c3
        assert_eq!(pawn_attacks(a2, Color::White).count(), 1); // b3
        assert_eq!(pawn_attacks(h2, Color::White).count(), 1); // g3
        assert_eq!(pawn_attacks(b7, Color::Black).count(), 2); // a6,c6
        assert_eq!(pawn_attacks(a7, Color::Black).count(), 1); // b6
        assert_eq!(pawn_attacks(h7, Color::Black).count(), 1); // g6
    }

    #[test]
    fn tables_match_generators() {
        for i in 0u8..64 { 
            let sq = Square::from_index(i).unwrap();
            assert_eq!(knight_attacks_tbl(sq), knight_attacks(sq));
            assert_eq!(king_attacks_tbl(sq), king_attacks(sq));
            assert_eq!(pawn_attacks_tbl(sq, Color::White), pawn_attacks(sq, Color::White));
            assert_eq!(pawn_attacks_tbl(sq, Color::Black), pawn_attacks(sq, Color::Black));
        }
    }

    #[test]
    fn rays_counts_basic() {
        let a1 = Square::from_index(0).unwrap();
        let d4 = Square::from_index(27).unwrap();
        assert_eq!(ray_e(a1).count(), 7);
        assert_eq!(ray_n(a1).count(), 7);
        assert_eq!(ray_ne(a1).count(), 7);
        assert_eq!(ray_w(a1).count(), 0);
        assert_eq!(ray_s(a1).count(), 0);
        assert_eq!(ray_nw(a1).count(), 0);
        assert_eq!(ray_se(a1).count(), 0);
        assert_eq!(ray_sw(a1).count(), 0);
        // d4 center
        assert_eq!(ray_n(d4).count(), 4);
        assert_eq!(ray_s(d4).count(), 3);
        assert_eq!(ray_e(d4).count(), 4);
        assert_eq!(ray_w(d4).count(), 3);
        assert_eq!(ray_ne(d4).count(), 4);
        assert_eq!(ray_nw(d4).count(), 3);
        assert_eq!(ray_se(d4).count(), 3);
        assert_eq!(ray_sw(d4).count(), 3);
    }

    #[test]
    fn empty_sliders_counts() {
        let a1 = Square::from_index(0).unwrap();
        let d4 = Square::from_index(27).unwrap();
        assert_eq!(rook_attacks_empty(a1).count(), 14);
        assert_eq!(bishop_attacks_empty(a1).count(), 7);
        assert_eq!(queen_attacks_empty(a1).count(), 21);
        assert_eq!(rook_attacks_empty(d4).count(), 14);
        assert_eq!(bishop_attacks_empty(d4).count(), 13);
        assert_eq!(queen_attacks_empty(d4).count(), 27);
    }

    #[test]
    fn rook_blocker_examples() {
        let d4 = Square::from_index(27).unwrap();
        // blockers: g4 (30) and d6 (43)
        let occ = bb_from_indices(&[30, 43]);
        let attacks = rook_attacks(occ, d4);
        // east: e4(28), f4(29), g4(30)
        assert!(attacks.test(Square::from_index(28).unwrap()));
        assert!(attacks.test(Square::from_index(29).unwrap()));
        assert!(attacks.test(Square::from_index(30).unwrap()));
        // west: c4(26), b4(25), a4(24)
        assert!(attacks.test(Square::from_index(26).unwrap()));
        assert!(attacks.test(Square::from_index(25).unwrap()));
        assert!(attacks.test(Square::from_index(24).unwrap()));
        // north: d5(35), d6(43)
        assert!(attacks.test(Square::from_index(35).unwrap()));
        assert!(attacks.test(Square::from_index(43).unwrap()));
        // south: d3(19), d2(11), d1(3)
        assert!(attacks.test(Square::from_index(19).unwrap()));
        assert!(attacks.test(Square::from_index(11).unwrap()));
        assert!(attacks.test(Square::from_index(3).unwrap()));
    }

    #[test]
    fn bishop_blocker_examples() {
        let d4 = Square::from_index(27).unwrap();
        // blockers: b6 (41) and f2 (13)
        let occ = bb_from_indices(&[41, 13]);
        let attacks = bishop_attacks(occ, d4);
        // NE: e5(36), f6(45), g7(54), h8(63)
        assert!(attacks.test(Square::from_index(36).unwrap()));
        assert!(attacks.test(Square::from_index(45).unwrap()));
        assert!(attacks.test(Square::from_index(54).unwrap()));
        assert!(attacks.test(Square::from_index(63).unwrap()));
        // NW stops at b6 (41): c5(34)
        assert!(attacks.test(Square::from_index(34).unwrap()));
        assert!(attacks.test(Square::from_index(41).unwrap()));
        // SE stops at f2 (13): e3(20)
        assert!(attacks.test(Square::from_index(13).unwrap()));
        assert!(attacks.test(Square::from_index(20).unwrap()));
        // SW to edge: c3(18), b2(9), a1(0)
        assert!(attacks.test(Square::from_index(18).unwrap()));
        assert!(attacks.test(Square::from_index(9).unwrap()));
        assert!(attacks.test(Square::from_index(0).unwrap()));
    }
}