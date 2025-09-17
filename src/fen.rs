//! FEN parsing (minimal): piece placement, side-to-move, castling rights, and en passant target.
//! This module parses the first four FEN fields (placement, active color, castling, ep). Halfmove/fullmove counters are ignored.
#![allow(dead_code)]

use std::fmt;

use crate::{square::Square, position::{Piece, PieceKind, Position}, Color};

/// Error returned when a FEN string is structurally invalid for this minimal parser.
#[derive(Debug, Clone)]
pub struct FenError {
  fen: String,
}

impl fmt::Display for FenError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "invalid FEN string: {}", self.fen)
    }
}

/// Parse a minimal FEN string: piece placement (field 1) and side-to-move (field 2).
///
/// Expectations:
/// - Exactly 8 ranks in field 1, separated by 7 slashes '/'.
/// - Each rank sums to exactly 8 files (pieces + digits).
/// - Field 2 is either "w" or "b".
///
/// Limitations (by design for now):
/// - Ignores castling rights, en passant, halfmove/fullmove counters.
pub fn fen_parser(_fen: &str) -> Result<Position, FenError> {
  let fen_string = _fen.to_string();
  let mut fen_parts = fen_string.split(' ');
  // Piece placement data
  let ppd = fen_parts.next().ok_or(FenError { fen: _fen.to_string() })?;
  // Active color
  let ac = fen_parts.next().ok_or(FenError { fen: _fen.to_string() })?;
  // Castling rights
  let cr = fen_parts.next().ok_or(FenError { fen: _fen.to_string() })?;
  // En Passant
  let ep = fen_parts.next().ok_or(FenError { fen: _fen.to_string() })?;

  let mut position = Position::empty();
  let mut file: u8 = 0; // 0..=7
  let mut rank: u8 = 7; // 7..=0 (start from rank 8 and go down)
  let mut slashes: u8 = 0; // must be exactly 7

  for c in ppd.chars() {
    match c {
      '1'..='8' => {
        let empty_squares = c.to_digit(10).unwrap() as u8;
        file = file.saturating_add(empty_squares);
        if file > 8 {
          return Err(FenError { fen: _fen.to_string() });
        }
      }
      '/' => {
        // End of a rank: must have exactly 8 files completed
        if file != 8 { return Err(FenError { fen: _fen.to_string() }); }
        slashes = slashes.saturating_add(1);
        if slashes > 7 { return Err(FenError { fen: _fen.to_string() }); }
        file = 0;
        // Move down one rank; if already at 0, underflow -> invalid
        if rank == 0 { return Err(FenError { fen: _fen.to_string() }); }
        rank -= 1;
      }
      'p' | 'P' | 'n' | 'N' | 'b' | 'B' | 'r' | 'R' | 'q' | 'Q' | 'k' | 'K' => {
        if file >= 8 { return Err(FenError { fen: _fen.to_string() }); }
        let sq = Square::from_file_rank(file, rank).ok_or(FenError { fen: _fen.to_string() })?;
        let piece = match c {
          'p' => Piece { kind: PieceKind::Pawn,   color: Color::Black },
          'P' => Piece { kind: PieceKind::Pawn,   color: Color::White },
          'n' => Piece { kind: PieceKind::Knight, color: Color::Black },
          'N' => Piece { kind: PieceKind::Knight, color: Color::White },
          'b' => Piece { kind: PieceKind::Bishop, color: Color::Black },
          'B' => Piece { kind: PieceKind::Bishop, color: Color::White },
          'r' => Piece { kind: PieceKind::Rook,   color: Color::Black },
          'R' => Piece { kind: PieceKind::Rook,   color: Color::White },
          'q' => Piece { kind: PieceKind::Queen,  color: Color::Black },
          'Q' => Piece { kind: PieceKind::Queen,  color: Color::White },
          'k' => Piece { kind: PieceKind::King,   color: Color::Black },
          'K' => Piece { kind: PieceKind::King,   color: Color::White },
          _ => unreachable!(),
        };
        position.set_piece(sq, piece);
        file += 1;
      }
      _ => return Err(FenError { fen: _fen.to_string() }),
    }
  }

  // After processing, we must be at the end of the last rank with exactly 8 files
  if !(slashes == 7 && rank == 0 && file == 8) {
    return Err(FenError { fen: _fen.to_string() });
  }

  position.stm = match ac {
    "w" => Color::White,
    "b" => Color::Black,
    _ => return Err(FenError { fen: _fen.to_string() }),
  };

  // Castling rights bitmask: WK=0b0001, WQ=0b0010, BK=0b0100, BQ=0b1000. '-' means none.
  for c in cr.chars() {
    match c {
      'K' => position.castling |= 0b0001,
      'Q' => position.castling |= 0b0010,
      'k' => position.castling |= 0b0100,
      'q' => position.castling |= 0b1000,
      '-' => {}, // no castling rights
      _ => return Err(FenError { fen: _fen.to_string() }),
    }
  }

  // En passant: '-' for none, otherwise algebraic like "e3". We store the target square if valid.
  if ep == "-" {
    position.ep = None;
  }
  else {
    let mut ep_copy = ep.chars();
    let file = ep_copy.next().unwrap();
    let rank = ep_copy.next().unwrap();

    position.ep = Square::from_file_rank(file as u8 - b'a', rank.to_digit(10).unwrap() as u8 - 1);
  }

  Ok(position)
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::square::Square;

  #[test]
  fn parse_startpos_minimal_fields_ok() {
    let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -";
    let pos = fen_parser(fen).expect("startpos should parse");
    assert_eq!(pos.stm, Color::White);
    // 32 pieces total
    assert_eq!(pos.occ_all.count(), 32);
    // Pawns on a2 and a7 present
    let a2 = Square::from_index(8).unwrap();
    let a7 = Square::from_index(48).unwrap();
    assert!(pos.occ_all.test(a2));
    assert!(pos.occ_all.test(a7));
  }

  #[test]
  fn parse_full_startpos_ignores_rest_ok() {
    let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
    let pos = fen_parser(fen).expect("full startpos should parse (we ignore extra fields)");
    assert_eq!(pos.occ_all.count(), 32);
    assert_eq!(pos.stm, Color::White);
  }

  #[test]
  fn parse_k_vs_k_black_to_move_ok() {
    let fen = "8/8/8/3k4/8/8/8/4K3 b KQkq -";
    let pos = fen_parser(fen).expect("K vs k should parse");
    assert_eq!(pos.stm, Color::Black);
    assert_eq!(pos.occ_all.count(), 2);
    // d5 (index 35) black king, e1 (index 4) white king
    assert!(pos.occ_all.test(Square::from_index(35).unwrap()));
    assert!(pos.occ_all.test(Square::from_index(4).unwrap()));
  }

  #[test]
  fn error_if_rank_count_wrong() {
    // 9 files on last rank -> error
    let fen = "8/8/8/8/8/8/8/9 w KQkq -";
    assert!(fen_parser(fen).is_err());
  }

  #[test]
  fn error_if_too_many_slashes() {
    let fen = "8/8/8/8/8/8/8/8/8 w KQkq -"; // 8 slashes => 9 ranks
    assert!(fen_parser(fen).is_err());
  }

  #[test]
  fn parse_castling_mask_and_no_ep() {
    let fen = "8/8/8/8/8/8/8/R3K2R w KQ -";
    let pos = fen_parser(fen).unwrap();
    assert_eq!(pos.castling & 0b1111, 0b0011, "expected WK|WQ");
    assert!(pos.ep.is_none());
  }

  #[test]
  fn parse_ep_square_ok() {
    // Black just played d7-d5 → ep target is d6
    let fen = "8/8/8/3pP3/8/8/8/8 w - d6";
    let pos = fen_parser(fen).unwrap();
    let d6 = Square::from_index(43).unwrap();
    assert_eq!(pos.ep, Some(d6));
  }
}