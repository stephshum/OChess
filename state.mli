open Command
open Models
(* [state] is a record describing a chess game. The board is an customizable
 *  12x12 grid where the 8x8 layout of classic chess is centered on the board.
 *  [missing] is a list of int 2-tuples where each tuple gives the
 *    location of an unplayable board position.
 *  [pieces] is a list of all possible types of pieces in the game and their
 *    move patterns.
 *  [pc_loc] is a list of 2-tuples of pieces and their locations.
 *  [captured] is a list of pc_loc that have been captured.
 *  [color] is a variant that tells whose turn it currently is
 *  [promote] is either some position of pawn to promote or none
 *  [trow] is the top row of the board
 *  [brow] is the bottom row of the board
 *  [turn] is the number of turns the game has been going on for, increments
 *    every white turn
 *  [score] is a 2-tuple of ints where the left is the score of white and the
 *    right is the score of black.
 *  [wking] is the position of the white king
 *  [bking] is the position of the black king
 *  [check] is an option of the player in check
 *  [checkmate] is an option of the player in checkmate; ends game *)
type state = {
  missing: position list;
  pieces: (name*(move list)) list;
  pc_loc: (position*(piece)) list;
  captured: piece list;
  color: color;
  promote: position option;
  trow: int;
  brow: int;
  turn: int;
  score: (int*int); (* left is White score, right is Black score *)
  wking: (int*int);
  bking: (int*int);
  check: color option;
  checkmate: color option
}

(* [do' c st] is [st'] if doing command [c] in state [st] results
 * in a new state [st'].  The function name [do'] is used because
 * [do] is a reserved keyword.  Define the "observable state" to
 * be all the information that is observable about the state
 * from the functions above that take a [state] as input.
 *   - The "move", "take", "shortcastle", "longcastle", and "promotion" commands
 *     result in an appropriately updated [st'], as described in M1: Design,
 *     if the action is valid in state [st].  If the action is invalid in
 *     state [st], the observable state remains unchanged in [st'].
 *       + The action of "move" is valid if the piece to be moved is
 *         capable of moving to the square it is to be moved to
 *         (according to the rules of chess) and if there is no piece on
 *         the square to be moved to
 *       + The action of "take" is valid if there is a piece to be
 *         taken where specified, and the action of "move" is valid for
 *         the taking piece given that the piece that the former has been taken
 *       + The action of "shortcastle" is valid if the king has never
 *         moved, the rook on the kingside has never moved, the
 *         squares between the king and the rook involved are unoccupied,
 *         the king is not in check, and the king does not cross over
 *         or end on a square in which it would be in check.
 *       + The action of "longcastle" is valid if the king has never
 *         moved, the rook on the queenside has never moved, the
 *         squares between the king and the rook involved are unoccupied,
 *         the king is not in check, and the king does not cross over
 *         or end on a square in which it would be in check.
 *       + The action of "promotion" is valid if the piece specified is a pawn
 *         that reaches its eighth rank by moving, and if the piece to be
 *         promoted to is neither a king nor a pawn.
 *   - The "captured" and "quit" commands are always possible and leave
 *     the observable state unchanged.
 *   - The behavior of [do'] is unspecified if the command is
 *     not one of the commands listed.
 * effects: none.  [do'] is not permitted to do any printing as
 *   part of implementing the REPL.  [do'] is not permitted to cause
 *   the engine to terminate.  [do'] is not permitted to raise an exception
 *   unless the precondition is violated.
 * requires: the input state was produced by [init_state],
 *   or by repeated applications of [do'] to such a state.
 * Citations: https://en.wikipedia.org/wiki/Castling
 *            https://en.wikipedia.org/wiki/Promotion_(chess)
*)
val do' : command -> state -> state

(* [init_state t] is the initial state of the game as
 * determined by an unspecified type.
 * requires: [t] give valid initial positions for pieces and a valid board *)
val init_state : Yojson.Basic.json -> state

(* [val_mov_lst pos st] is the list of valid movement for a piece at [pos]
 * requires: [pos] is a valid board position containing a player's piece *)
val val_move_lst : position -> state -> position list
