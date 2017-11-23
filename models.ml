type position = int * int

type move =
  | Jump of position
  | Up
  | Right
  | DiagR
  | DiagL
  | PawnMov of bool
  | KingMov of bool

type color =
  | Black
  | White

type name =
  | Pawn
  | Rook of bool
  | Knight
  | Bishop
  | Queen
  | King
  | Custom of string

type piece = {
  name: name;
  pcolor: color;
  pattern: move list;
}

type power =
  | RaisetheDead
  | Elimination
  | NoJumpers
  | SecondChance
  | Clone
  | MindControl
  | CultMurder

type powerup = {
  pow_name: power;
  place: position list;
  persists: bool;
}
