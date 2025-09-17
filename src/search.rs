#![allow(dead_code)]

use std::cmp::max;

use crate::{eval::eval, move_gen::{Move, MoveGen}, position::Position, Color};

pub fn search_bestmove(pos: &mut Position, depth: u32) -> Option<Move> {
  let moves = pos.legal_moves();
  let mut best_move: Option<Move> = None;
  let mut best_eval = i32::MIN + 1;

  for mv in moves {
    let undo = pos.make_unchecked(&mv);
    let move_eval = -negamax(pos, depth - 1, i32::MIN + 1, i32::MAX - 1);
    pos.unmake(&mv, undo);
    if move_eval > best_eval {
      best_move = Some(mv);
      best_eval = move_eval;
    }
  }
  return best_move;
}

fn negamax (pos: &mut Position, depth: u32, mut alpha: i32, mut beta: i32) -> i32 {
  if depth == 0 {
    return eval(pos);
  }
  let moves = pos.legal_moves();
  if moves.len() == 0 {
    let king_square = pos.king_square(pos.stm).unwrap();
    if  pos.is_square_attacked_by(king_square, if pos.stm == Color::White {Color::Black} else {Color::White}) {
      return -30_000;
    }
    else {
      return 0;
    }
  }

  let mut score: i32 = i32::MIN ;
  for mv in moves {
    let undo = pos.make_unchecked(&mv);
    score = max(score, -negamax(pos, depth - 1, -beta, -alpha));
    pos.unmake(&mv, undo);
    alpha = max(alpha, score);
    if alpha >= beta {
      break;
    }
  }
  return score;
}