#![allow(dead_code)]

use crate::{move_gen::MoveGen, position::Position};


/// Count legal move leaf nodes to a given depth.
/// perft(pos, 0) = 1; for d>0, sum perft(child, d-1) over all legal moves.
pub fn perft(pos: &Position, depth: u32) -> u64 {
  if depth == 0 { return 1; }
  let mut clone = *pos;
  let moves = clone.legal_moves();
  let mut nodes = 0;
  for mv in moves {
    let undo = clone.make_unchecked(&mv);
    nodes += perft(&clone, depth - 1);
    clone.unmake(&mv, undo);
  }
  nodes
}

/// Debugging helper: perft divide
/// Prints each legal move from the root position with its leaf node count at depth-1.
pub fn perft_divide(pos: &Position, depth: u32) {
  assert!(depth > 0, "divide only makes sense for depth > 0");
  let mut clone = *pos;
  eprintln!("Castling mask: {}", clone.castling);
  let moves = clone.legal_moves();
  let mut total = 0;
  for mv in moves {
    let undo = clone.make_unchecked(&mv);
    let cnt = perft(&clone, depth - 1);
    clone.unmake(&mv, undo);
    eprintln!("{:?} -> {}", mv, cnt);
    total += cnt;
  }
  eprintln!("Total: {}", total);
}

/// Aggregate counters for perft diagnostics.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct PerftStats {
  pub nodes: u64,
  pub captures: u64,
  pub eps: u64,
  pub checks: u64,
}

impl PerftStats {
  fn add(&mut self, other: PerftStats) {
    self.nodes += other.nodes;
    self.captures += other.captures;
    self.eps += other.eps;
    self.checks += other.checks;
  }
}

/// Perft with extra breakdown: counts nodes and, at the final ply, how many
/// moves are captures, en-passant captures, and give check.
/// Convention: for depth D, we count events on the last move (when `depth == 1`).
pub fn perft_stats(pos: &Position, depth: u32) -> PerftStats {
  if depth == 0 {
    return PerftStats { nodes: 1, ..Default::default() };
  }
  let mut clone = *pos;
  let moves = clone.legal_moves();

  if depth == 1 {
    let mut stats = PerftStats::default();
    for mv in moves {
      // Make once to check capture kind and whether the move gives check.
      let undo = clone.make_unchecked(&mv);
      stats.nodes += 1; // each legal move at the last ply contributes one leaf
      if undo.captured.is_some() || mv.ep { stats.captures += 1; }
      if mv.ep { stats.eps += 1; }
      // Check if the move gives check to the opponent (now `clone.stm` is the opponent)
      let opp_king_sq = clone.king_square(clone.stm).expect("opponent king must exist");
      let mover = if crate::color_idx(clone.stm) == 0 { crate::Color::Black } else { crate::Color::White };
      if clone.is_square_attacked_by(opp_king_sq, mover) { stats.checks += 1; }
      clone.unmake(&mv, undo);
    }
    return stats;
  }

  // depth > 1: recurse
  let mut total = PerftStats::default();
  for mv in moves {
    let undo = clone.make_unchecked(&mv);
    let child = perft_stats(&clone, depth - 1);
    clone.unmake(&mv, undo);
    total.add(child);
  }
  total
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::fen::fen_parser;

  #[test]
  fn perft_depth0_is_one() {
    let pos = fen_parser("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -").unwrap();
    assert_eq!(perft(&pos, 0), 1);
  }

  #[test]
  fn startpos_perft_depth1() {
    let pos = fen_parser("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -").unwrap();
    assert_eq!(perft(&pos, 1), 20);
  }

  #[test]
  fn startpos_perft_depth2() {
    let pos = fen_parser("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -").unwrap();
    assert_eq!(perft(&pos, 2), 400);
  }

  #[test]
  fn startpos_perft_depth3() {
    let pos = fen_parser("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -").unwrap();
    assert_eq!(perft(&pos, 3), 8902);
  }

  #[test]
  fn startpos_perft_depth4() {
    let pos = fen_parser("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -").unwrap();
    assert_eq!(perft(&pos, 4), 197_281);
  }

  #[test]
  fn startpos_perft_depth5() {
    let pos = fen_parser("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -").unwrap();
    assert_eq!(perft(&pos, 5), 4_865_609);
  }

  // Kiwipete position – exercises castling, EP, promotions interactions
  // r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -
  #[test]
  fn kiwipete_perft_depth1() {
    let pos = fen_parser("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -").unwrap();
    assert_eq!(perft(&pos, 1), 48);
  }

  #[test]
  fn kiwipete_perft_depth2() {
    let pos = fen_parser("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -").unwrap();
    assert_eq!(perft(&pos, 2), 2_039);
  }

  #[test]
  fn kiwipete_perft_depth3() {
    let pos = fen_parser("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -").unwrap();
    assert_eq!(perft(&pos, 3), 97_862);
  }

  #[test]
  fn kiwipete_perft_depth4() {
    let pos = fen_parser("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -").unwrap();
    assert_eq!(perft(&pos, 4), 4_085_603);
  }

  #[test]
  fn talkchess_perft_depth1() {
    let pos = fen_parser("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8").unwrap();
    assert_eq!(perft(&pos, 1), 44);
  }

  #[test]
  fn talkchess_perft_depth2() {
    let pos = fen_parser("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8").unwrap();
    assert_eq!(perft(&pos, 2), 1_486);
  }

  #[test]
  fn talkchess_perft_depth3() {
    let pos = fen_parser("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8").unwrap();
    assert_eq!(perft(&pos, 3), 62_379);
  }

  #[test]
  fn talkchess_perft_depth4() {
    let pos = fen_parser("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8").unwrap();
    assert_eq!(perft(&pos, 4), 2_103_487);
  }

  #[test]
  #[ignore]
  fn talkchess_perft_depth5() {
    let pos = fen_parser("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8").unwrap();
    assert_eq!(perft(&pos, 5), 89_941_194);
  }

  #[test]
  #[ignore]
  fn kiwipete_divide_depth1() {
    let pos = fen_parser("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -").unwrap();
    perft_divide(&pos, 1);
  }
  #[test]
  #[ignore]
  fn kiwipete_stats_depth2() {
    let pos = fen_parser("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -").unwrap();
    let stats = super::perft_stats(&pos, 2);
    eprintln!("KIWIPETE d2 => nodes: {} captures: {} ep: {} checks: {}", stats.nodes, stats.captures, stats.eps, stats.checks);
  }
}