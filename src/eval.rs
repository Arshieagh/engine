#![allow(dead_code)]

use crate::{color_idx, position::{PieceKind, Position}, square::Square, Color};

const P: i32 = 100;
const N: i32 = 320;
const B: i32 = 330;
const R: i32 = 500;
const Q: i32 = 900;

fn get_pieces_score(pos: &Position, color: Color) -> i32 {
  let mut score: i32 = 0;
  let mut pieces = pos.occ_side[color_idx(color)].clone();
  while let Some(piece_idx) = pieces.pop_lsb() {
    let piece = pos.piece_at(Square::from_index(piece_idx).unwrap()).unwrap();
    match piece.kind {
      PieceKind::Pawn => { score += P }
      PieceKind::Knight => { score += N }
      PieceKind::Bishop => { score += B }
      PieceKind::Rook => { score += R }
      PieceKind::Queen => { score += Q }
      _ => {}
    }
  }
  score
}

pub fn eval(pos: &Position) -> i32 {
  let p_score_w = get_pieces_score(pos, Color::White);
  let p_score_b = get_pieces_score(pos, Color::Black);
  match pos.stm {
    Color::White => {p_score_w - p_score_b}
    Color::Black => {p_score_b - p_score_w}
  }
}