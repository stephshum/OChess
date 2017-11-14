(* [state] is an abstract type representing the state of a chess game. *)
type state

(* [do' c st] is [st'] if doing command [c] in state [st] results
 * in a new state [st'].  The function name [do'] is used because
 * [do] is a reserved keyword.  Define the "observable state" to
 * be all the information that is observable about the state
 * from the functions above that take a [state] as input.
 *   - The "move", "take", "shortcastle", "longcastle", and "promotion" commands
 *     result in an appropriately updated [st'], as described below
 *     (*TODO: need to know that things are included in the state*),
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
 *         that reaches its eighth rank by moving can be edited due to customizability (*TODO*)
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
val do' : Command.command -> state -> state

(* [init_state j] is the initial state of the game as
 * determined by JSON object [j].
 * requires: [j] represents an error-free adventure file. *)
val init_state : Yojson.Basic.json -> state

(* [can_shortcastle s] is true if player can shortcastle *)
val can_shortcastle : state -> bool

(* [can_longcastle s] is true if player can longcastle *)
val can_longcastle : state -> bool

(*TODO may merge can_longcastle and can_shortcastle to val_move*)

(* [val_mov s st] is true if [s] is a valid player move
 * requires: [s] is a tuple of positions, the first being the piece to move and
 *  the second being the position to move to *)
val val_move : ((int*int)*(int*int)) -> state -> bool

(* [can_promote s] is true ig player can promote *)
val can_promote : state -> bool

(* [in_check s] is true if player is in check *)
val in_check : state -> bool

(* [in_checkmate s] is true if player is in checkmate *)
val in_checkmate : state -> bool

(* [score s] is the player's current score. *)
val score : state -> int

(* [turns s] is the number of turns the player has taken so far. *)
val turns : state -> int
