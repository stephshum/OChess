type position = int * int

type move =
  | Jump of position
  | Up
  | Right
  | DiagR
  | DiagL
  | PawnMov
  | KingMov

type color =
  | Black
  | White

type name =
  | Pawn
  | Rook
  | Knight
  | Bishop
  | Queen
  | King
  | Custom of string

type piece = {
  name: name;
  color: color;
  pattern: move list;
}

type powerup = {
  name: string;
  place: position list;
  persists: bool;
}
