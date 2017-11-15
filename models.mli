(* [position] represents the placement of a piece on the chess board denoted
 * by a string for its horizontal coordinate and an int for its vertical
 * coordinate on the board. *)
type position = int * int

(* [type] represents possible movements that a piece can make in its overall
 * movement pattern. *)
type move =
  | Jump of position * position
  | Up
  | Left

type color =
  | Black
  | White

(* [piece] represents a  *)
type piece = {
  name: string;
  color: color;
  pattern: move list;
  mutable initial: position;
}

type powerup = {
  name: string;
  place: position list;
  persists: bool;
}

type void = {
  place: position list;
  persists: bool;
}
