open Models

(* [command] represents a command input given by a change in the GUI made
 * by a player. *)
type command =
  | Move of (position * position)
  | Promotion of name
  | Quit
  | Invalid
  | PreMove of position

(* [parse str] is the command that represents changes made by the player
 * to the board. *)
val parse : string -> command
