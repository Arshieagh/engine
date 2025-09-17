//! Position: board state (piece bitboards per side/kind) and basic helpers.
//! Minimal shell for occupancy, set/clear, and piece lookup.
#![allow(dead_code)]

use crate::attacks::{bishop_attacks, king_attacks_tbl, knight_attacks_tbl, pawn_attacks_tbl, rook_attacks};
use crate::move_gen::Castling;
use crate::square::Square;
use crate::{bitboard::Bitboard, Color, color_idx, move_gen::Move};

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct Undo {
  pub captured: Option<Piece>,
  pub prev_stm: Color,
  pub castled: bool,
  pub prev_castling: u8,
  pub prev_ep: Option<Square>,
}

/// The logical kind of a chess piece (no color).
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum PieceKind {
  Pawn,
  Knight,
  Bishop,
  Rook,
  Queen,
  King,
}


/// Map a 0..=5 index to a `PieceKind` (0=P,1=N,2=B,3=R,4=Q,5=K).
impl PieceKind {
  pub fn from_index(s: usize) -> Option<PieceKind> {
      match s {
        0 => Some(PieceKind::Pawn),
        1 => Some(PieceKind::Knight),
        2 => Some(PieceKind::Bishop),
        3 => Some(PieceKind::Rook),
        4 => Some(PieceKind::Queen),
        5 => Some(PieceKind::King),
        _ => None,
      }
  }
}
  /// Map a `PieceKind` to its 0..=5 index (P=0, N=1, B=2, R=3, Q=4, K=5).
#[inline]
pub fn piece_kind_idx(pk:PieceKind) -> usize {
  match pk {
    PieceKind::Pawn => 0,
    PieceKind::Knight => 1,
    PieceKind::Bishop => 2,
    PieceKind::Rook => 3,
    PieceKind::Queen => 4,
    PieceKind::King => 5,
  }
}

/// A concrete piece on the board: its kind and color.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Piece {
  pub kind: PieceKind,
  pub color: Color,
}

/// The board state: per-side/per-kind bitboards, derived occupancies, and side-to-move.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Position {
    pub bb: [[Bitboard; 6]; 2], //[color][kind] bitboards
    pub occ_side: [Bitboard; 2], //derived (per side)
    pub occ_all: Bitboard,  //derived (all)
    pub stm: Color, //side to move
    pub castling: u8, //castling rights (bit 1=WK,2=WQ,4=BK,8=BQ)
    pub ep: Option<Square>, // En Passant availability
}

impl Position {
  /// Create an empty position (no pieces), side to move defaults to White.
  pub fn empty() -> Self {
    Self {
      bb: [[Bitboard::empty(); 6]; 2],
      occ_side: [Bitboard::empty(); 2],
      occ_all: Bitboard::empty(),
      stm: Color::White,
      castling: 0b0000,
      ep: None
    }
  }

  /// Place `piece` on `sq`. Updates per-kind, per-side, and overall occupancy.
  pub fn set_piece(&mut self, sq: Square, piece: Piece) {
    let col_idx: usize = color_idx(piece.color);
    let piece_idx: usize = piece_kind_idx(piece.kind);
    self.bb[col_idx][piece_idx].set(sq);
    self.occ_side[col_idx].set(sq);
    self.occ_all.set(sq);
  }

  /// Remove any piece on `sq` (if present) from all bitboards and occupancies.
  pub fn clear_piece(&mut self, sq: Square) {
    let s = !(1u64 << sq.index());
    for pk in 0..6 {
      self.bb[0][pk].0 &= s;
      self.bb[1][pk].0 &= s;
    }
    self.occ_side[0].0 &= s;
    self.occ_side[1].0 &= s;
    self.occ_all.0 &= s;
  }

  /// Recompute `occ_side` and `occ_all` from the per-kind bitboards.
  pub fn recompute_occupancy(&mut self) {
    self.occ_side[0] = self.bb[0].iter().fold(Bitboard::empty(), |acc, bb| acc | *bb);
    self.occ_side[1] = self.bb[1].iter().fold(Bitboard::empty(), |acc, bb| acc | *bb);
    self.occ_all = self.occ_side[0] | self.occ_side[1];
  }

  /// Occupancy for the opponent of the current side-to-move (STM-relative).
  pub fn enemy_occ(&self) -> Bitboard {
    self.occ_side[color_idx(self.stm) ^ 1]
  }

  /// Return the piece at `sq` if any by scanning per-side/per-kind bitboards.
  pub fn piece_at(&self, sq: Square) -> Option<Piece> {
    let s = 1u64 << sq.index();
    if (s & self.occ_all.0) == 0 { return None; }
    else {
      for c in 0..2 {
        for pk in 0..6 {
          if s & self.bb[c][pk].0 != 0 {
            return Some(Piece { kind: PieceKind::from_index(pk).unwrap(), color: Color::from_index(c).unwrap() });
          }
        }
      }
    }
    None
  }

  /// Locate the king square for the given color (LSB if somehow multiple).
  pub fn king_square(&self, color: Color) -> Option<Square> {
    let pk = piece_kind_idx(PieceKind::King);
    let col_idx = color_idx(color);
    self.bb[col_idx][pk].lsb().and_then(Square::from_index)
  }

  /// Toggle side to move.
  pub fn toggle_stm(&mut self) {
    self.stm = match self.stm {
      Color::White => Color::Black,
      Color::Black => Color::White,
    };
  }

  /// Returns true iff `sq` is attacked by pieces of color `by`.
/// Uses reverse masks for pawns and symmetric masks for knights/kings.
/// For sliders we compute attacks FROM `sq` with current occupancy and see
/// if the first blocker in any ray is a bishop/rook/queen of `by`.
pub fn is_square_attacked_by(&self, sq: Square, by: Color) -> bool {
    let by_idx = color_idx(by);

    // Knights
    let knights = self.bb[by_idx][piece_kind_idx(PieceKind::Knight)];
    if (knight_attacks_tbl(sq) & knights).any() {
        return true;
    }

    // Kings
    let kings = self.bb[by_idx][piece_kind_idx(PieceKind::King)];
    if (king_attacks_tbl(sq) & kings).any() {
        return true;
    }

    // Pawns (reverse mask): to know if `sq` is attacked by `by` pawns,
    // use the opposite-color mask FROM `sq`, then AND with `by` pawns.
    let opp = if by_idx == 0 { Color::Black } else { Color::White };
    let pawn_rev = pawn_attacks_tbl(sq, opp);
    let pawns = self.bb[by_idx][piece_kind_idx(PieceKind::Pawn)];
    if (pawn_rev & pawns).any() {
        return true;
    }

    // Sliders: rays from `sq`
    let bishops_q = self.bb[by_idx][piece_kind_idx(PieceKind::Bishop)]
                  | self.bb[by_idx][piece_kind_idx(PieceKind::Queen)];
    let rooks_q   = self.bb[by_idx][piece_kind_idx(PieceKind::Rook)]
                  | self.bb[by_idx][piece_kind_idx(PieceKind::Queen)];

    if (bishop_attacks(self.occ_all, sq) & bishops_q).any() {
        return true;
    }
    if (rook_attacks(self.occ_all, sq) & rooks_q).any() {
        return true;
    }

    false
  }
  /// Update castling rights after a move.
  /// Rules:
  /// - Any king move clears both rights for that color.
  /// - A rook move from its original square clears that side's corresponding right.
  /// - A capture on a rook's original square clears the victim side's right.
  /// 
  /// Note: This function is called once per `make_unchecked`.
  pub fn update_castling(&mut self, piece: Piece, mv: &Move, undo: Undo) {
    if piece.kind == PieceKind::King {
      match piece.color {
          Color::White => { self.castling &= 0b1100 }
          Color::Black => { self.castling &= 0b0011 }
      }
    }
    else if piece.kind == PieceKind::Rook {
      if piece.color == Color::White && mv.from.rank() == 0 {
        match mv.from.file() {
            // Loses queen side capturing rights
            0 => { self.castling &= 0b1101 }
            // Loses king side capturing rights
            7 => { self.castling &= 0b1110 }
            _ => {}
        }
      }
      if piece.color == Color::Black && mv.from.rank() == 7 {
        match mv.from.file() {
            // Loses queen side capturing rights
            0 => { self.castling &= 0b0111 }
            // Loses king side capturing rights
            7 => { self.castling &= 0b1011 }
            _ => {}
        }
      }
    }
    else if undo.captured.is_some() {
      let captured_piece = undo.captured.unwrap();
      if captured_piece.kind == PieceKind::Rook {
        // King side rook is captured
        if mv.to.file() == 7 {
          match captured_piece.color {
            Color::White => { if mv.to.rank() == 0 { self.castling &= 0b1110 }}
            Color::Black => { if mv.to.rank() == 7 { self.castling &= 0b1011 }}
          }
        }
        // Queen side rook is captured
        else if mv.to.file() == 0 {
          match captured_piece.color {
            Color::White => { if mv.to.rank() == 0 { self.castling &= 0b1101 }}
            Color::Black => { if mv.to.rank() == 7 { self.castling &= 0b0111 }}
          }
        }
      }
    }
  }

  /// Apply a pseudo-legal move without legality checks. Handles captures and promotions.
  /// Returns an `Undo` record sufficient for `unmake` to restore the state.
  pub fn make_unchecked(&mut self, mv: &Move) -> Undo {
    let from = mv.from;
    let to = mv.to;
    let promotion = mv.promotion;
    let castling = mv.castle;
    let piece = self.piece_at(from).unwrap();
    let to_piece = self.piece_at(to);

    let undo: Undo;

    // Snapshot the previous en-passant target before we mutate it
    let old_ep = self.ep;
    self.ep = None;
    // Simple Move (explicitly not en passant)
    if to_piece.is_none() && promotion.is_none() && castling.is_none() && !mv.ep {
      if mv.from.rank().abs_diff(mv.to.rank()) > 1 && piece.kind == PieceKind::Pawn {
        match piece.color {
            Color::White => { self.ep = from.offset(8) }
            Color::Black => { self.ep = from.offset(-8) }
        }
      }
      self.clear_piece(from);
      self.set_piece(to, piece);
      self.recompute_occupancy();
      // If this was a pawn double push, set the en-passant target to the jumped-over square.
      if piece.kind == PieceKind::Pawn {
          let from_idx = from.index();
          let to_idx   = to.index();
          if piece.color == Color::White {
              // White double from rank 2 (index +16)
              if to_idx == from_idx + 16 {
                  let ep_sq = Square::from_index(from_idx + 8).unwrap(); // e3 after e2e4
                  self.ep = Some(ep_sq);
              }
          } else {
              // Black double from rank 7 (index -16)
              if from_idx == to_idx + 16 {
                  let ep_sq = Square::from_index(from_idx - 8).unwrap(); // d6 after d7d5
                  self.ep = Some(ep_sq);
              }
          }
      }
      undo = Undo {
        captured: None,
        prev_stm: self.stm,
        castled: false,
        prev_castling: self.castling,
        prev_ep: old_ep,
      };
      self.toggle_stm();
    }

    // Capture move
    else if to_piece.is_some() && promotion.is_none() && castling.is_none() || mv.ep {
      if mv.ep {
        let to_ep = match piece.color {
          Color::White => { old_ep.unwrap().offset(-8) }
          Color::Black => { old_ep.unwrap().offset(8) }
        }.unwrap();

        undo = Undo {
          captured: self.piece_at(to_ep),
          prev_stm: self.stm,
          castled: false,
          prev_castling: self.castling,
          prev_ep: old_ep,
        };
        self.clear_piece(from);
        self.clear_piece(to_ep);
        self.set_piece(to, piece);
      }
      else {
        self.clear_piece(from);
        self.clear_piece(to);
        self.set_piece(to, piece);
        undo = Undo {
          captured: to_piece,
          prev_stm: self.stm,
          castled: false,
          prev_castling: self.castling,
          prev_ep: old_ep,
        };
      }
      self.recompute_occupancy();
      self.toggle_stm();
    }
    // Simple Promotion Move
    else if to_piece.is_none() && promotion.is_some() && castling.is_none() {
      self.clear_piece(from);
      self.set_piece(to, Piece { kind:promotion.unwrap(), color: self.stm});
      self.recompute_occupancy();
      undo = Undo {
        captured: None,
        prev_stm: self.stm,
        castled: false,
        prev_castling: self.castling,
        prev_ep: old_ep,
      };
      self.toggle_stm();
    }
    // Capture Promotion Move
    else if to_piece.is_some() && promotion.is_some() && castling.is_none() {
      self.clear_piece(from);
      self.clear_piece(to);
      self.set_piece(to, Piece { kind:promotion.unwrap(), color: self.stm});
      self.recompute_occupancy();
      undo = Undo {
        captured: to_piece,
        prev_stm: self.stm,
        castled: false,
        prev_castling: self.castling,
        prev_ep: old_ep,
      };
      self.toggle_stm();
    }
    // Castling: king moves two squares; also move the rook accordingly (handled here, not as a separate Move)
    else {
      let rook = Piece {kind: PieceKind::Rook, color: piece.color};
      self.clear_piece(from);
      self.set_piece(to, piece);
      if castling.unwrap() == Castling::KingSide {
        self.clear_piece(from.offset(3).unwrap());
        self.set_piece(to.offset(-1).unwrap(), rook);
      }
      else {
        self.clear_piece(from.offset(-4).unwrap());
        self.set_piece(to.offset(1).unwrap(), rook);
      }
      self.recompute_occupancy();
      undo = Undo {
        captured: None,
        prev_stm: self.stm,
        castled: true,
        prev_castling: self.castling,
        prev_ep: old_ep,
      };
      self.toggle_stm();
    }
    self.update_castling(piece, mv, undo);
    undo
  }

  /// Revert a move previously applied with `make_unchecked`, restoring STM and any captured piece.
  pub fn unmake(&mut self, mv: &Move, undo: Undo) {
    let from = mv.from;
    let to = mv.to;
    let promotion = mv.promotion;
    let piece = self.piece_at(to);
    let captured = undo.captured;
    
    // Simple Move
    if captured.is_none() && promotion.is_none() && !undo.castled {
      self.clear_piece(to);
      self.set_piece(from, piece.unwrap());
    }

    // Capture Move (not en passant)
    else if captured.is_some() && promotion.is_none() && !mv.ep {
      self.clear_piece(to);
      self.set_piece(from, piece.unwrap());
      self.set_piece(to, captured.unwrap());
    }

    // Simple Promotion Move
    else if captured.is_none() && promotion.is_some() {
      self.clear_piece(to);
      self.set_piece(from, Piece { kind: PieceKind::Pawn, color: undo.prev_stm });
    }

    // Capture Promotion Move
    else if captured.is_some() && promotion.is_some() {
      self.clear_piece(to);
      self.set_piece(from, Piece { kind: PieceKind::Pawn, color: undo.prev_stm });
      self.set_piece(to, captured.unwrap());
    }

    // En passant unmake: use `undo.prev_ep` to reconstruct the captured pawn square
    else if captured.is_some() && mv.ep {
      self.clear_piece(to);
      self.set_piece(from, Piece { kind: PieceKind::Pawn, color: undo.prev_stm });
      let to_ep = match piece.unwrap().color {
          Color::White => { undo.prev_ep.unwrap().offset(-8) }
          Color::Black => { undo.prev_ep.unwrap().offset(8) }
        }.unwrap();
      self.set_piece(to_ep, captured.unwrap());
    }

    // Castling Move: move the rook back and restore the king to the origin
    else if mv.castle.is_some() {
      // Side that castled is the previous side to move
      let mover_color = undo.prev_stm;
      let king = Piece { kind: PieceKind::King, color: mover_color };
      let rook = Piece { kind: PieceKind::Rook, color: mover_color };

      // Remove the king from destination square
      self.clear_piece(to);

      match mv.castle.unwrap() {
        Castling::KingSide => {
          // Rook moved from h-file to f-file during make
          let rook_from = to.offset(-1).unwrap(); // f1/f8
          let rook_to = from.offset(3).unwrap();  // h1/h8
          self.clear_piece(rook_from);
          self.set_piece(rook_to, rook);
        }
        Castling::QueenSide => {
          // Rook moved from a-file to d-file during make
          let rook_from = to.offset(1).unwrap();  // d1/d8
          let rook_to = from.offset(-4).unwrap(); // a1/a8
          self.clear_piece(rook_from);
          self.set_piece(rook_to, rook);
        }
      }

      // Put the king back on its origin
      self.set_piece(from, king);
    }

    // Recompute occupancy and restore side to move and castling rights
    self.recompute_occupancy();
    self.stm = undo.prev_stm;
    self.castling = undo.prev_castling;
    self.ep = undo.prev_ep;
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn empty_position_has_no_pieces() {
    let p = Position::empty();
    assert!(p.occ_all.is_empty());
    assert!(p.occ_side[0].is_empty());
    assert!(p.occ_side[1].is_empty());
  }

  #[test]
  fn set_and_get_pieces() {
    let mut p = Position::empty();
    let e1 = Square::from_index(4).unwrap();   // a1=0 .. e1=4
    let e8 = Square::from_index(60).unwrap();  // e8 index
    p.set_piece(e1, Piece { kind: PieceKind::King, color: Color::White });
    p.set_piece(e8, Piece { kind: PieceKind::King, color: Color::Black });

    assert_eq!(p.occ_side[0].count(), 1);
    assert_eq!(p.occ_side[1].count(), 1);
    assert_eq!(p.occ_all.count(), 2);

    let w = p.piece_at(e1).unwrap();
    assert_eq!(w.kind, PieceKind::King);
    assert_eq!(w.color, Color::White);
    let b = p.piece_at(e8).unwrap();
    assert_eq!(b.kind, PieceKind::King);
    assert_eq!(b.color, Color::Black);
  }

  #[test]
  fn clear_piece_works() {
    let mut p = Position::empty();
    let a1 = Square::from_index(0).unwrap();
    p.set_piece(a1, Piece { kind: PieceKind::Rook, color: Color::White });
    assert!(p.occ_all.any());
    p.clear_piece(a1);
    assert!(p.occ_all.is_empty());
    assert!(p.piece_at(a1).is_none());
  }

  #[test]
  fn recompute_occupancy_matches_incremental() {
    let mut p = Position::empty();
    let d4 = Square::from_index(27).unwrap();
    let h8 = Square::from_index(63).unwrap();
    p.set_piece(d4, Piece { kind: PieceKind::Queen, color: Color::White });
    p.set_piece(h8, Piece { kind: PieceKind::Bishop, color: Color::Black });

    // Snapshot counts after incremental updates
    let w_cnt = p.occ_side[0].count();
    let b_cnt = p.occ_side[1].count();
    let all_cnt = p.occ_all.count();

    // Zero and recompute
    p.occ_side = [Bitboard::empty(), Bitboard::empty()];
    p.occ_all = Bitboard::empty();
    p.recompute_occupancy();

    assert_eq!(p.occ_side[0].count(), w_cnt);
    assert_eq!(p.occ_side[1].count(), b_cnt);
    assert_eq!(p.occ_all.count(), all_cnt);
  }

  #[test]
  fn make_unmake_simple_move_roundtrip() {
    let mut p = Position::empty();
    // White knight g1 -> e2
    let g1 = Square::from_index(6).unwrap();
    let e2 = Square::from_index(12).unwrap();
    p.set_piece(g1, Piece { kind: PieceKind::Knight, color: Color::White });
    let before = p; // copy
    let mv = Move { from: g1, to: e2, promotion: None, castle: None, ep: false };
    let undo = p.make_unchecked(&mv);
    // After make: knight on e2, stm toggled
    assert!(p.piece_at(e2).is_some());
    assert!(p.piece_at(g1).is_none());
    // Unmake: restore
    p.unmake(&mv, undo);
    assert_eq!(p, before);
  }

  #[test]
  fn make_unmake_capture_roundtrip() {
    let mut p = Position::empty();
    // White bishop c4 captures black pawn f7
    let c4 = Square::from_index(26).unwrap();
    let f7 = Square::from_index(53).unwrap();
    p.set_piece(c4, Piece { kind: PieceKind::Bishop, color: Color::White });
    p.set_piece(f7, Piece { kind: PieceKind::Pawn, color: Color::Black });
    let before = p;
    let mv = Move { from: c4, to: f7, promotion: None, castle: None, ep: false };
    let undo = p.make_unchecked(&mv);
    assert_eq!(p.piece_at(f7), Some(Piece { kind: PieceKind::Bishop, color: Color::White }));
    p.unmake(&mv, undo);
    assert_eq!(p, before);
  }

  #[test]
  fn make_unmake_promotion_push_roundtrip() {
    let mut p = Position::empty();
    // White pawn a7 -> a8 = Q
    let a7 = Square::from_index(48).unwrap();
    let a8 = Square::from_index(56).unwrap();
    p.set_piece(a7, Piece { kind: PieceKind::Pawn, color: Color::White });
    let before = p;
    let mv = Move { from: a7, to: a8, promotion: Some(PieceKind::Queen), castle: None, ep: false };
    let undo = p.make_unchecked(&mv);
    assert_eq!(p.piece_at(a8), Some(Piece { kind: PieceKind::Queen, color: Color::White }));
    p.unmake(&mv, undo);
    assert_eq!(p, before);
  }

  #[test]
  fn make_unmake_promotion_capture_roundtrip() {
    let mut p = Position::empty();
    // White pawn a7 x b8 = Q (capture)
    let a7 = Square::from_index(48).unwrap();
    let b8 = Square::from_index(57).unwrap();
    p.set_piece(a7, Piece { kind: PieceKind::Pawn, color: Color::White });
    p.set_piece(b8, Piece { kind: PieceKind::Rook, color: Color::Black });
    let before = p;
    let mv = Move { from: a7, to: b8, promotion: Some(PieceKind::Queen), castle: None, ep: false };
    let undo = p.make_unchecked(&mv);
    assert_eq!(p.piece_at(b8), Some(Piece { kind: PieceKind::Queen, color: Color::White }));
    p.unmake(&mv, undo);
    assert_eq!(p, before);
  }

  #[test]
  fn is_square_attacked_by_basic_cases() {
    let mut p = Position::empty();
    // White king on d1
    let d1 = Square::from_index(3).unwrap();
    p.set_piece(d1, Piece { kind: PieceKind::King, color: Color::White });
    // Black queen on d5 (same file), clear path
    let d5 = Square::from_index(35).unwrap();
    p.set_piece(d5, Piece { kind: PieceKind::Queen, color: Color::Black });
    // d1 should be attacked by Black
    assert!(p.is_square_attacked_by(d1, Color::Black));
    // Move queen off the file and recheck
    p.clear_piece(d5);
    let c5 = Square::from_index(34).unwrap();
    p.set_piece(c5, Piece { kind: PieceKind::Queen, color: Color::Black });
    assert!(!p.is_square_attacked_by(d1, Color::Black));
  }

  #[test]
  fn is_square_attacked_by_knight_and_pawn() {
    let mut p = Position::empty();
    // White king on d1
    let d1 = Square::from_index(3).unwrap();
    p.set_piece(d1, Piece { kind: PieceKind::King, color: Color::White });
    // Black knight on e3 attacks d1
    let e3 = Square::from_index(20).unwrap();
    p.set_piece(e3, Piece { kind: PieceKind::Knight, color: Color::Black });
    assert!(p.is_square_attacked_by(d1, Color::Black));
    // Replace knight with a black pawn on e2 (which attacks d1 as Black)
    p.clear_piece(e3);
    p.recompute_occupancy();
    let e2 = Square::from_index(12).unwrap();
    p.set_piece(e2, Piece { kind: PieceKind::Pawn, color: Color::Black });
    assert!(p.is_square_attacked_by(d1, Color::Black));
  }
}

  #[test]
  fn castling_make_unmake_white_kingside() {
    // Setup: White pieces on e1/h1 with K right only; empty f1,g1
    let mut p = Position::empty();
    let e1 = Square::from_index(4).unwrap();
    let f1 = Square::from_index(5).unwrap();
    let g1 = Square::from_index(6).unwrap();
    let h1 = Square::from_index(7).unwrap();
    p.set_piece(e1, Piece { kind: PieceKind::King, color: Color::White });
    p.set_piece(h1, Piece { kind: PieceKind::Rook, color: Color::White });
    p.castling = 0b0001; // WK only
    let before = p;

    let mv = Move { from: e1, to: g1, promotion: None, castle: Some(Castling::KingSide), ep: false };
    let undo = p.make_unchecked(&mv);
    // After castle: king g1, rook f1, white rights cleared
    assert_eq!(p.piece_at(g1), Some(Piece { kind: PieceKind::King, color: Color::White }));
    assert_eq!(p.piece_at(f1), Some(Piece { kind: PieceKind::Rook, color: Color::White }));
    assert!(p.piece_at(e1).is_none());
    assert!(p.piece_at(h1).is_none());

    // Unmake should restore everything, including castling mask
    p.unmake(&mv, undo);
    assert_eq!(p, before);
  }

  #[test]
  fn castling_make_unmake_white_queenside() {
    // Setup: White pieces on e1/a1 with Q right only; empty d1,c1,b1
    let mut p = Position::empty();
    let e1 = Square::from_index(4).unwrap();
    let d1 = Square::from_index(3).unwrap();
    let c1 = Square::from_index(2).unwrap();
    let a1 = Square::from_index(0).unwrap();
    p.set_piece(e1, Piece { kind: PieceKind::King, color: Color::White });
    p.set_piece(a1, Piece { kind: PieceKind::Rook, color: Color::White });
    p.castling = 0b0010; // WQ only
    let before = p;

    let mv = Move { from: e1, to: c1, promotion: None, castle: Some(Castling::QueenSide), ep: false };
    let undo = p.make_unchecked(&mv);
    // After castle: king c1, rook d1
    assert_eq!(p.piece_at(c1), Some(Piece { kind: PieceKind::King, color: Color::White }));
    assert_eq!(p.piece_at(d1), Some(Piece { kind: PieceKind::Rook, color: Color::White }));
    assert!(p.piece_at(e1).is_none());
    assert!(p.piece_at(a1).is_none());

    p.unmake(&mv, undo);
    assert_eq!(p, before);
  }

  #[test]
  fn castling_make_unmake_black_kingside() {
    let mut p = Position::empty();
    let e8 = Square::from_index(60).unwrap();
    let f8 = Square::from_index(61).unwrap();
    let g8 = Square::from_index(62).unwrap();
    let h8 = Square::from_index(63).unwrap();
    p.set_piece(e8, Piece { kind: PieceKind::King, color: Color::Black });
    p.set_piece(h8, Piece { kind: PieceKind::Rook, color: Color::Black });
    p.castling = 0b0100; // BK only
    p.toggle_stm();
    let before = p;

    let mv = Move { from: e8, to: g8, promotion: None, castle: Some(Castling::KingSide), ep: false };
    let undo = p.make_unchecked(&mv);
    assert_eq!(p.piece_at(g8), Some(Piece { kind: PieceKind::King, color: Color::Black }));
    assert_eq!(p.piece_at(f8), Some(Piece { kind: PieceKind::Rook, color: Color::Black }));
    assert!(p.piece_at(e8).is_none());
    assert!(p.piece_at(h8).is_none());

    p.unmake(&mv, undo);
    assert_eq!(p, before);
  }

  #[test]
  fn castling_make_unmake_black_queenside() {
    let mut p = Position::empty();
    let e8 = Square::from_index(60).unwrap();
    let d8 = Square::from_index(59).unwrap();
    let c8 = Square::from_index(58).unwrap();
    let a8 = Square::from_index(56).unwrap();
    p.set_piece(e8, Piece { kind: PieceKind::King, color: Color::Black });
    p.set_piece(a8, Piece { kind: PieceKind::Rook, color: Color::Black });
    p.castling = 0b1000; // BQ only
    p.toggle_stm();
    let before = p;

    let mv = Move { from: e8, to: c8, promotion: None, castle: Some(Castling::QueenSide), ep: false };
    let undo = p.make_unchecked(&mv);
    assert_eq!(p.piece_at(c8), Some(Piece { kind: PieceKind::King, color: Color::Black }));
    assert_eq!(p.piece_at(d8), Some(Piece { kind: PieceKind::Rook, color: Color::Black }));
    assert!(p.piece_at(e8).is_none());
    assert!(p.piece_at(a8).is_none());

    p.unmake(&mv, undo);
    assert_eq!(p, before);
  }

  #[test]
  fn update_castling_clears_rights_on_king_and_rook_moves() {
    let mut p = Position::empty();
    // White full rights to start, king on e1, rooks on a1/h1
    let e1 = Square::from_index(4).unwrap();
    let a1 = Square::from_index(0).unwrap();
    let h1 = Square::from_index(7).unwrap();
    p.set_piece(e1, Piece { kind: PieceKind::King, color: Color::White });
    p.set_piece(a1, Piece { kind: PieceKind::Rook, color: Color::White });
    p.set_piece(h1, Piece { kind: PieceKind::Rook, color: Color::White });
    p.castling = 0b0011; // WK|WQ

    // Move rook from a1 -> a2 (should clear WQ)
    let mv_rq = Move { from: a1, to: a1.offset(8).unwrap(), promotion: None, castle: None, ep: false };
    let undo_rq = Undo { captured: None, prev_stm: Color::White, castled: false, prev_castling: p.castling, prev_ep: None };
    p.update_castling(Piece { kind: PieceKind::Rook, color: Color::White }, &mv_rq, undo_rq);
    assert_eq!(p.castling & 0b0010, 0, "WQ should be cleared");

    // Reset and move rook from h1 -> h2 (should clear WK)
    p.castling = 0b0011;
    let mv_rk = Move { from: h1, to: h1.offset(8).unwrap(), promotion: None, castle: None, ep: false };
    let undo_rk = Undo { captured: None, prev_stm: Color::White, castled: false, prev_castling: p.castling, prev_ep: None };
    p.update_castling(Piece { kind: PieceKind::Rook, color: Color::White }, &mv_rk, undo_rk);
    assert_eq!(p.castling & 0b0001, 0, "WK should be cleared");

    // Reset and move king e1 -> e2 (should clear both)
    p.castling = 0b0011;
    let mv_k = Move { from: e1, to: e1.offset(8).unwrap(), promotion: None, castle: None, ep: false };
    let undo_k = Undo { captured: None, prev_stm: Color::White, castled: false, prev_castling: p.castling, prev_ep: None };
    p.update_castling(Piece { kind: PieceKind::King, color: Color::White }, &mv_k, undo_k);
    assert_eq!(p.castling & 0b0011, 0, "Both white rights should be cleared");
  }