open Command
open Models
open Yojson.Basic.Util


type state = {
  missing: position list;
  pieces: (position*(piece)) list;
  captured: piece list;
  color: color;
  promote: bool;
  turn: int;
  score: (int*int);
  check: color option;
  checkmate: color option
}


let init_state j =
  let re = Str.regexp "[0-9]+" in
  let int_tuple_of_json j =
    let s = j |> to_string in
    let pos = Str.search_forward re s 0 in
    let fs = Str.matched_string s in
    let nxt = (String.sub s (String.length fs+pos)
                 (String.length s - String.length fs-pos)) in
    let _ = Str.search_forward re nxt 0 in
    let sn = Str.matched_string nxt in
    (fs|>int_of_string,sn|>int_of_string)
  in
  let color_of_json j =
    let s = j |> to_string in
    if s = "Black" then Black
    else White
  in
  let name_of_json j =
    let s = j |> to_string in
    if s = "Pawn" then Pawn
    else if s = "Rook" then Rook
    else if s = "Knight" then Knight
    else if s = "Bishop" then Bishop
    else if s = "Queen" then Queen
    else if s = "King" then King
    else Custom s
  in
  let pattern_of_json j =
    let s = j |> to_string in
    if s = "Up" then Up
    else if s = "Right" then Right
    else if s = "DiagR" then DiagR
    else if s = "DiagL" then DiagL
    else if s = "Pawn" then PawnMov true
    else if s = "King" then KingMov true
    else Jump (j |> int_tuple_of_json)
  in
  let piece_of_json j = {
    name = j |> member "name" |> name_of_json;
    color = j |> member "color" |> color_of_json;
    pattern = j |> member "pattern" |> to_list |> List.map pattern_of_json;
  }
  in
  let pieces_of_json j =
    let piece = j |> member "piece" |> piece_of_json in
    let position = j |> member "position" |> int_tuple_of_json in
    (position,piece)
  in
  let check_of_json j =
    let s = j |> to_string in
    if s = "None" then None
    else if s = "Black" then Some Black
    else Some White
  in
  let checkmate_of_json j =
    let s = j |> to_string in
    if s = "None" then None
    else if s = "Black" then Some Black
    else Some White
  in
  {
    missing = j |> member "missing" |> to_list |> List.map int_tuple_of_json;
    pieces = j |> member "pieces" |> to_list |> List.map pieces_of_json;
    captured = j |> member "captured" |> to_list |> List.map piece_of_json;
    color = j |> member "color" |> color_of_json;
    promote = j |> member "promote" |> to_bool;
    turn = j |> member "turn" |> to_int;
    score = j |> member "score" |> int_tuple_of_json;
    check = j |> member "check" |> check_of_json;
    checkmate = j |> member "promote" |> checkmate_of_json
  }


let val_move_lst (x,y) st =
  let pc = List.assoc (x,y) st.pieces in
  let check_pos (a,b) acc =
    if (a > 11 || a < 0 || b > 11 || b < 0) then acc
    else
      if List.mem (a,b) st.missing then acc
      else
        try
          if (List.assoc (a,b) st.pieces).color = pc.color then acc
          else (a,b)::acc
        with
        | Not_found -> (a,b)::acc
  in
  let rec movs_of_vec (xs,ys) (vx,vy) inv (acc:'a list) =
    if not inv then
      let nx = x+vx in
      let ny = y+vy in
      let new_acc = check_pos (nx,ny) acc in
      if new_acc = acc then movs_of_vec (x,y) (vx,vy) true new_acc
      else
        try
          let _ = List.assoc (nx,ny) st.pieces in
          movs_of_vec (x,y) (vx,vy) true new_acc
        with
        | Not_found -> movs_of_vec (x,y) (vx,vy) false new_acc
    else
      let nx = -1*(x+vx) in
      let ny = -1*(y+vy) in
      let new_acc = check_pos (nx,ny) acc in
      if new_acc = acc then acc
      else
        try
          let _ = List.assoc (nx,ny) st.pieces in
          new_acc
        with
        | Not_found -> movs_of_vec (x,y) (vx,vy) true new_acc
  in
  let check_fr dir =
    if List.mem (x,y+dir) st.missing then
      if List.mem_assoc (x,y+dir) st.pieces then
        []
      else
        [(x,y+dir)]
    else
      []
  in
  let check_diag (x',y') acc =
    if ((List.mem_assoc (x',y') st.pieces) &&
       (List.assoc (x',y') st.pieces).color <> pc.color) then
      (x,y)::acc
    else
      acc
  in
  let pawn_mov dir =
    if y+dir > 11 || y+dir < 0 then
      let fr = check_fr dir in
      let lf = check_diag (x-1,y+dir) fr in
      check_diag (x+1,y+dir) lf
    else failwith "Impossible"
  in
  let king_move _ =
    let a = check_pos (x-1,y) [] in
    let b = check_pos (x-1,y+1) a in
    let c = check_pos (x-1,y-1) b in
    let d = check_pos (x,y+1) c in
    let e = check_pos (x,y-1) d in
    let f = check_pos (x+1,y-1) e in
    let g = check_pos (x+1,y+1) f in
    let h = check_pos (x+1,y) g in
    failwith "Unimplemented"
  in
  List.fold_left (
    fun acc mv ->
      match mv with
      | Jump (x',y') ->
        (if pc.color = White then check_pos (x+x',y+y') acc
         else check_pos (x+x',-1*(y+y')) acc)
      | Up -> movs_of_vec (x,y) (0,-1) false acc
      | Right -> movs_of_vec (x,y) (1,0) false acc
      | DiagR -> movs_of_vec (x,y) (1,1) false acc
      | DiagL -> movs_of_vec (x,y) (-1,1) false acc
      | PawnMov b -> if pc.color = White then pawn_mov (-1) else pawn_mov 1
      | KingMov b -> king_move ()
  ) [] pc.pattern

let do' cmd st =
  failwith "Unimplemented"
