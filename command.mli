open Models

(* [command] represents a command input given by a change in the GUI made
 * by a player. *)
type command =
  | Move of position * position
  | Take of position * position
  | ShortCastle
  | LongCastle
  | Promotion of position * piece
  | Captured
  | Quit
  | Invalid






(* [parse str] is the command that represents player input [str].
 * requires: [str] is one of the commands forms described in the
 *   assignment writeup. *)
val parse : string -> command
