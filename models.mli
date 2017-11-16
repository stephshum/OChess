(* [position] represents the placement of a piece on the chess board denoted
 * first by an int for its horizontal coordinate and then an int for its vertical
 * coordinate on the board. *)
type position = int * int

(* [move] represents possible movements that a piece can make in its overall
 * movement pattern. *)
type move =
  | Jump of position * position
  | Up
  | Right
  | DiagR
  | DiagL
  | Pawn
  | King

(* [color] represents the color of the piece, whether it is black or white. *)
type color =
  | Black
  | White

(* [name] represents either the name of a piece in the classic chess game or
 * a custom piece created by a player. *)
type name =
  | Pawn of piece
  | Rook of piece
  | Knight of piece
  | Bishop of piece
  | Queen of piece
  | King of piece
  | Custom of piece

(* [piece] represents a playable chess piece and contains information on the  *)
and piece = {
  name: name;
  color: color;
  pattern: move list;
  mutable initial: position;
}

(* [powerup] represents the type of a powerup that pieces can gain if their
 * position is the same as the placement of the powerup on the board. The
 * powerup may stay on the board at the same position after or may not. *)
type powerup = {
  name: string;
  place: position list;
  persists: bool;
}

(* [void] represents the type of a void in the board where pieces are not
 * allowed to move. *)
type void = position list

(* [move_pattern p] is the list of possible movement patterns [piece] can make *)
val move_pattern: piece -> move list
