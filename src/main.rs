mod square;
mod bitboard;
mod attacks;
mod position;
mod fen;
mod move_gen;
mod perft;
mod uci;
mod eval;
mod search;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Color {
  White,
  Black,
}

impl Color {
  pub fn from_index(s: usize) -> Option<Color> {
    match s {
      0 => Some(Color::White),
      1 => Some(Color::Black),
      _ => None,
    }
  }
}

#[inline]
fn color_idx(c:Color) -> usize {
  match c {
    Color::White => 0,
    Color::Black => 1,
  }
}

fn main() {
    let mut eng = uci::UciEngine::new();
    eng.run_stdio();
}
