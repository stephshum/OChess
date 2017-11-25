open Models

type command =
  | Move of position * position
  | Promotion of piece
  | Quit
  | Invalid

let parse s =
  failwith "Unimplemented"
