#![allow(dead_code)]

//! Minimal UCI shell (stdin/stdout) with a small, well-commented scaffold.
//! Supported commands for now:
//! - `uci` → id/author + `uciok`
//! - `isready` → `readyok`
//! - `ucinewgame` → reset position to startpos
//! - `position startpos [moves ...]` or `position fen <FEN 6 fields> [moves ...]`
//! - `go perft N` → prints the total node count
//! - `quit` → return from the run loop
//!
//! This module keeps parsing/handlers very explicit (no async, no threads).

use std::io::{self, BufRead, Write};

use crate::fen::fen_parser;
use crate::perft::perft;
use crate::position::{Position, PieceKind};
use crate::move_gen::{Move, Castling};
use crate::{Color};
use crate::square::Square; // assuming you have a Square type in a square module
use crate::search::search_bestmove;

/// The UCI engine state. Owns the current position and lightweight engine info.
pub struct UciEngine {
  pub pos: Position,
  pub header: String,
}

impl UciEngine {
  /// Create a new engine with an empty position (caller will load startpos/fen).
  pub fn new() -> Self {
    // If you have a helper for startpos, you can use it. We'll start empty.
    Self { pos: fen_parser("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -").expect("startpos FEN must parse"), header: "rusty-chess".to_string() }
  }

  /// Main read-eval-print loop for UCI over stdin/stdout.
  /// Returns when it sees `quit`.
  pub fn run_stdio(&mut self) {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    for line in stdin.lock().lines() {
      let Ok(mut line) = line else { break }; // EOF or error ends the loop
      line = line.trim().to_string();
      if line.is_empty() { continue; }
      if self.handle_line(&line, &mut stdout) { break; }
    }
  }

  /// Handle a single UCI command line. Returns true if we should quit.
  fn handle_line(&mut self, line: &str, out: &mut dyn Write) -> bool {
    // We keep tokenization simple and robust.
    let mut it = line.split_whitespace();
    let Some(cmd) = it.next() else { return false };
    match cmd {
      "uci" => {
        writeln!(out, "id name {}", self.header).ok();
        writeln!(out, "id author your-name").ok();
        // later: writeln!(out, "option name Hash type spin default 16 min 1 max 4096").ok();
        writeln!(out, "uciok").ok();
        out.flush().ok();
      }
      "isready" => {
        writeln!(out, "readyok").ok();
        out.flush().ok();
      }
      "ucinewgame" => {
        // Reset game-specific state. We'll also clear the board for safety.
        self.pos = fen_parser("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -").expect("startpos FEN must parse");
      }
      "position" => {
        // Reconstruct a position and optionally apply a moves list.
        let rest: Vec<&str> = it.collect();
        self.handle_position(&rest);
      }
      "go" => {
        let rest: Vec<&str> = it.collect();
        self.handle_go(&rest, out);
      }
      "quit" => {
        return true;
      }
      _ => {
        // Silently ignore unknown commands for now.
      }
    }
    false
  }

  /// `position` command handler.
  /// Accepts: `startpos [moves ...]` or `fen <6 fields> [moves ...]`.
  fn handle_position(&mut self, tokens: &[&str]) {
    if tokens.is_empty() { return; }

    if tokens[0] == "startpos" {
      self.pos = fen_parser("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -").expect("startpos FEN must parse");
      // Optional moves
      if let Some(idx) = tokens.iter().position(|&t| t == "moves") {
        for mv_str in &tokens[idx+1..] {
          if let Some(mv) = self.parse_uci_move(mv_str) { let _ = self.pos.make_unchecked(&mv); }
        }
      }
      return;
    }

    if tokens[0] == "fen" {
      // Need at least 6 fields after `fen`
      if tokens.len() < 7 { return; }
      let fen_fields = tokens[1..7].join(" ");
      if let Ok(p) = fen_parser(&fen_fields) { self.pos = p; }
      // Optional moves
      if let Some(idx) = tokens.iter().position(|&t| t == "moves") {
        for mv_str in &tokens[idx+1..] {
          if let Some(mv) = self.parse_uci_move(mv_str) { let _ = self.pos.make_unchecked(&mv); }
        }
      }
    }
  }

  /// `go` command handler. Currently supports `go perft N` and `go depth N`.
  fn handle_go(&mut self, tokens: &[&str], out: &mut dyn Write) {
    if tokens.len() >= 2 && tokens[0] == "perft" {
      if let Ok(depth) = tokens[1].parse::<u32>() {
        let start = std::time::Instant::now();
        let n = perft(&self.pos, depth);
        let elapsed = start.elapsed();
        writeln!(
            out,
            "nodes {} time {} ms nps {}",
            n,
            elapsed.as_millis(),
            (n as f64 / elapsed.as_secs_f64()) as u64
        ).ok();
        let _ = out.flush();
        return;
      }
    }

    if tokens.len() >= 2 && tokens[0] == "depth" {
      if let Ok(depth) = tokens[1].parse::<u32>() {
        let mut pos = self.pos.clone();
        if let Some(bm) = search_bestmove(&mut pos, depth) {
          let uci = self.move_to_uci(&bm);
          writeln!(out, "bestmove {}", uci).ok();
          let _ = out.flush();
          // also update our internal position to keep playing if the GUI expects it
          let _ = self.pos.make_unchecked(&bm);
        }
      }
    }
  }

  /// Parse a UCI coord move like `e2e4`, `e7e8q`, `e1g1`, `e5d6` (ep if applicable).
  /// Uses the **current position** to infer castle and en-passant flags.
  fn parse_uci_move(&self, s: &str) -> Option<Move> {
    if s.len() < 4 { return None; }
    let bytes = s.as_bytes();
    let from = self.parse_square(&bytes[0..2])?;
    let to   = self.parse_square(&bytes[2..4])?;

    // Optional promotion piece (q/r/b/n)
    let mut promo: Option<PieceKind> = None;
    if s.len() == 5 {
      promo = match bytes[4] as char {
        'q' | 'Q' => Some(PieceKind::Queen),
        'r' | 'R' => Some(PieceKind::Rook),
        'b' | 'B' => Some(PieceKind::Bishop),
        'n' | 'N' => Some(PieceKind::Knight),
        _ => None,
      };
    }

    // Castle detection: king e1→g1/c1 or e8→g8/c8
    let mut castle = None;
    if let Some(king_sq) = self.pos.king_square(self.pos.stm) {
      if from == king_sq {
        // White side
        if self.pos.stm == Color::White {
          if from.index() == 4 && to.index() == 6 { castle = Some(Castling::KingSide); }
          if from.index() == 4 && to.index() == 2 { castle = Some(Castling::QueenSide); }
        } else {
          if from.index() == 60 && to.index() == 62 { castle = Some(Castling::KingSide); }
          if from.index() == 60 && to.index() == 58 { castle = Some(Castling::QueenSide); }
        }
      }
    }

    // En-passant detection: pawn diagonal to the current ep target
    let mut ep_flag = false;
    if let Some(ep_sq) = self.pos.ep {
      // A legal EP must move a pawn diagonally into `ep_sq`.
      let pawns = self.pos.bb[crate::color_idx(self.pos.stm)][crate::position::piece_kind_idx(PieceKind::Pawn)];
      // from must contain a pawn of stm
      if pawns.test(from) && to == ep_sq {
        // and the move must be diagonal (files differ by 1, ranks by 1)
        let ff = (from.file() as i8 - to.file() as i8).abs();
        let rf = (from.rank() as i8 - to.rank() as i8).abs();
        if ff == 1 && rf == 1 { ep_flag = true; }
      }
    }

    Some(Move { from, to, promotion: promo, castle, ep: ep_flag })
  }

  /// Encode a Move into a UCI coord string (e.g., e2e4, e7e8q). Castling is just e1g1/e1c1/e8g8/e8c8.
  fn move_to_uci(&self, mv: &Move) -> String {
    fn sq_to_str(sq: Square) -> [u8; 2] {
      let f = (b'a' + sq.file() as u8) as u8;
      let r = (b'1' + sq.rank() as u8) as u8;
      [f, r]
    }
    let mut s = Vec::with_capacity(5);
    let a = sq_to_str(mv.from);
    let b = sq_to_str(mv.to);
    s.extend_from_slice(&a);
    s.extend_from_slice(&b);
    if let Some(pk) = mv.promotion {
      let ch = match pk {
        PieceKind::Queen  => b'q',
        PieceKind::Rook   => b'r',
        PieceKind::Bishop => b'b',
        PieceKind::Knight => b'n',
        _ => b'q', // should not happen; only Q/R/B/N are legal promotions
      };
      s.push(ch);
    }
    String::from_utf8(s).unwrap()
  }

  /// Parse a square like `e2` into `Square`.
  fn parse_square(&self, two: &[u8]) -> Option<Square> {
    if two.len() != 2 { return None; }
    let file = two[0];
    let rank = two[1];
    if !(b'a'..=b'h').contains(&file) { return None; }
    if !(b'1'..=b'8').contains(&rank) { return None; }
    let f = file - b'a';
    let r = rank - b'1';
    let idx = (r as u16) * 8 + (f as u16);
    Square::from_index(idx as u8)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn parse_square_ok() {
    let eng = UciEngine::new();
    assert_eq!(eng.parse_square(b"a1"), Square::from_index(0));
    assert_eq!(eng.parse_square(b"h1"), Square::from_index(7));
    assert_eq!(eng.parse_square(b"a8"), Square::from_index(56));
    assert_eq!(eng.parse_square(b"h8"), Square::from_index(63));
  }

  #[test]
  fn parse_moves_castle_promo_ep() {
    // Start from a simple FEN where we can test parsing logic in isolation.
    let mut eng = UciEngine::new();
    eng.pos = fen_parser("r3k2r/8/8/8/8/8/8/R3K2R w KQkq -").unwrap();

    // White O-O
    let m1 = eng.parse_uci_move("e1g1").unwrap();
    assert!(matches!(m1.castle, Some(Castling::KingSide)));
    assert!(!m1.ep);

    // Promotion
    eng.pos = fen_parser("8/P7/8/8/8/8/8/8 w - -").unwrap();
    let m2 = eng.parse_uci_move("a7a8q").unwrap();
    assert_eq!(m2.promotion, Some(PieceKind::Queen));

    // EP: set up e2e4 then black d7d5 so ep is d6, white e5xd6 e.p.
    eng.pos = fen_parser("8/8/8/3pP3/8/8/8/8 w - d6").unwrap();
    let m3 = eng.parse_uci_move("e5d6").unwrap();
    assert!(m3.ep);
  }

  #[test]
  fn move_to_uci_formats_correctly() {
    let mut eng = UciEngine::new();
    eng.pos = fen_parser("r3k2r/8/8/8/8/8/8/R3K2R w KQkq -").unwrap();
    let m1 = eng.parse_uci_move("e1g1").unwrap();
    assert_eq!(eng.move_to_uci(&m1), "e1g1");

    eng.pos = fen_parser("8/P7/8/8/8/8/8/8 w - -").unwrap();
    let m2 = eng.parse_uci_move("a7a8q").unwrap();
    assert_eq!(eng.move_to_uci(&m2), "a7a8q");
  }
}
