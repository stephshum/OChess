(* [position] represents the placement of a piece on the chess board denoted
 * first by an int for its horizontal coordinate and then an int for its vertical
 * coordinate on the board. *)
type position = int * int

(* [move] represents possible movements that a piece can make in its overall
 * movement pattern. *)
type move =
  | Jump of position
  | Up
  | Right
  | DiagR
  | DiagL
  | PawnMov
  | KingMov

(* [color] represents the color of the piece, whether it is black or white. *)
type color =
  | Black
  | White

(* [name] represents either the name of a piece in the classic chess game or
 * a custom piece created by a player. All custom piece names must be lowercase!
 *)
type name =
  | Pawn of bool
  | Rook of bool
  | Knight
  | Bishop
  | Queen
  | King of bool
  | Custom of string

(* [piece] represents a chess piece and its color *)
type piece = {
  name: name;
  pcolor: color;
}

(* [power] represent the name of a powerup *)
type power =
  | RaisetheDead
  | Elimination
  | NoJumpers
  | SecondChance
  | Clone
  | MindControl
  | CultMurder

(* [powerup] represents the type of a powerup that pieces can gain if their
 * position is the same as the placement of the powerup on the board. The
 * powerup may stay on the board at the same position after or may not. *)
type powerup = {
  pow_name: power;
  place: position list;
  persists: bool;
}
