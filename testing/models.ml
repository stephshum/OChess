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
  | Pawn of bool
  | Rook of bool
  | Knight
  | Bishop
  | Queen
  | King of bool
  | Custom of string

type piece = {
   name: name;
   pcolor: color;
 }

type power =
  | RaisetheDead
  | Elimination
  | NoJumpers
  | SecondChance
  | Clone
  | MindControl
  | CultMurder

(*TODO might remove this*)
(* type powerup = {
  pow_name: power;
  place: position list;
  persists: bool;
} *)
