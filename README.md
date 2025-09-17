# Rust Chess Engine + Streamlit GUI

This project combines a **chess engine written in Rust** (speaking the [UCI protocol](https://en.wikipedia.org/wiki/Universal_Chess_Interface)) with a **web-based GUI built in Python using [Streamlit](https://streamlit.io/)**.

- **Rust side** → High-performance search and evaluation, compiled as a standalone UCI binary.
- **Python side** → Lightweight web interface to play moves, visualize the board, and debug engine behavior in real time.

---

## Architecture
  +—————–——————–+        stdin/stdout (UCI)       +—————–——————–+
|   Streamlit GUI   |  <––––––––––––––––––––>  |   Rust Engine    |
|  (Python server)  |                          |  (binary, UCI)   |
  +—————–——————–+                                 +—————–——————–+
- **Communication:**  
  - The GUI launches the Rust engine as a subprocess.  
  - Messages are exchanged in UCI format (`uci`, `isready`, `position`, `go`, `info`, `bestmove`, …).  
  - The Python side manages the protocol state and renders live search info.

- **Session state:**  
  - Each browser session has its own engine instance.  
  - Streamlit keeps the engine handle in `st.session_state` (or similar).  

---

## Getting Started

### Prerequisites
Rust toolchain (`cargo`, `rustc`)  
Python 3.9+  
Dependencies:
  ```bash
  pip install streamlit python-chess
```  
## Build the Engine

Compile your Rust chess engine into a binary:
  ```bash
  cargo build --release
```
The binary should live at ./target/release/my_rust_uci_engine (update path as needed).

## Run the GUI
  ```bash
  streamlit run gui.py
```
This will start a local server (usually at http://localhost:8501).

---

## Debugging the Engine
Streamlit is not just a GUI—it’s also a debugger console for your engine.
What you can montior:
- Raw UCI transcript: every command sent, every line received.
- Search telemetry from info lines:
- Depth / selective depth
- Nodes searched / NPS
- Score (centipawns, mate-in-N)
- Principal variation (PV)
- Position state: FEN, move list, side to move.
- Engine options: hash size, threads, etc.

Typical workflows:
1. Perft checks → validate move generation with known counts.
2. Fixed-depth search → run the same FEN at depth N and compare PVs/scores for stability.
3. Edge cases → test castling, promotions, en-passant, stalemates.
4. Time management → check if go movetime is respected.
5. Crash triage → if bestmove never arrives, inspect protocol transcript for missing flush/newline.

 ## Resources
 [UCI Protocol Specification](https://gist.github.com/aliostad/f6c19dba0f5a1c0e6f0c)
 [python-chess docs](https://python-chess.readthedocs.io/en/latest/)
 [Streamlit documentation](https://docs.streamlit.io/)

 
