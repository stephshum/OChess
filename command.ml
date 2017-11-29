open Models

type command =
  | Move of position * position
  | Promotion of name
  | Quit
  | Invalid
  | PreMove of position

let parse s =
  failwith "Unimplemented"
