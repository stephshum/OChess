(* [command] represents a command input given by a change in the GUI made
 * by a player. *)
type command =
  | Move of Models.position * Models.position
  | Take of Models.position * Models.position
  | ShortCastle
  | LongCastle
  | Promotion of Models.position * Models.piece
  | Captured
  | Quit
  | Invalid






(* [parse str] is the command that represents player input [str].
 * requires: [str] is one of the commands forms described in the
 *   assignment writeup. *)
val parse : string -> command
