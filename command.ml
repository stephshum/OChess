open Models

type command =
  | Move of position * position
  | Promotion of position * piece
  | Captured
  | Quit
  | Invalid

let parse s =
  failwith "Unimplemented"
