open Command
open Models
open Yojson.Basic.Util

type state = {
  missing: position list;
  pieces: (position*(piece)) list;
  captured: piece list;
  color: color;
  promote: position option;
  trow: int;
  brow: int;
  turn: int;
  score: (int*int); (* left is White score, right is Black score *)
  wking: (int*int);
  bking: (int*int);
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
    else if s = "Rook" then Rook true
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
    pcolor = j |> member "color" |> color_of_json;
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
    promote = None;
    trow = j |> member "trow" |> to_int;
    brow = j |> member "brow" |> to_int;
    turn = j |> member "turn" |> to_int;
    score = j |> member "score" |> int_tuple_of_json;
    wking = j |> member "wking" |> int_tuple_of_json;
    bking = j |> member "bking" |> int_tuple_of_json;
    check = j |> member "check" |> check_of_json;
    checkmate = j |> member "promote" |> checkmate_of_json
  }

(* [in_check miss pcs clr kloc] true if [clr] king is in check
 * requires:
 *  [miss] is a valid list of missing board positions
 *  [pcs] is a valid list of pieces with at least king of both colors
 *  [clr] is the color of the king to check
 *  [kloc] is the location of the king whose color is given by [clr] *)
let in_check miss pcs clr kloc =
  let rec king_in_vec (kx,ky) (x,y) (x',y') (vx,vy) inv =
    if not inv then
      let nx = x'+vx in
      let ny = y'+vy in
      if (nx > 11 || nx < 0 || ny > 11 || ny < 0) then
        king_in_vec (kx,ky) (x,y) (x,y) (vx,vy) true
      else if List.mem (nx,ny) miss then
        king_in_vec (kx,ky) (x,y) (x,y) (vx,vy) true
      else if List.mem_assoc (nx,ny) pcs then
        if (nx,ny) = (kx,ky) then true
        else king_in_vec (kx,ky) (x,y) (x,y) (vx,vy) true
      else
        king_in_vec (kx,ky) (x,y) (nx,ny) (vx,vy) false
    else
      let nx = -1*(x'+vx) in
      let ny = -1*(y'+vy) in
      if (nx > 11 || nx < 0 || ny > 11 || ny < 0) then false
      else if List.mem (nx,ny) miss then false
      else if List.mem_assoc (nx,ny) pcs then
        if (nx,ny) = (kx,ky) then true else false
      else
        king_in_vec (kx,ky) (x,y) (nx,ny) (vx,vy) true
  in
  let pking (kx,ky) (x,y) f =
    (kx,ky) = (x-1,y+f) || (kx,ky) = (x+1,y+f)
  in
  let kking (kx,ky) (x,y) =
    (kx,ky) = (x,y-1) ||
    (kx,ky) = (x,y+1) ||
    (kx,ky) = (x-1,y-1) ||
    (kx,ky) = (x-1,y+1) ||
    (kx,ky) = (x-1,y) ||
    (kx,ky) = (x+1,y-1) ||
    (kx,ky) = (x+1,y+1) ||
    (kx,ky) = (x+1,y)
  in
  let rec check_helper (x,y) pcs colr  =
    match pcs with
    | [] -> false
    | ((x',y'),{name=_;pcolor=colr;pattern=p;})::t ->
      begin
        List.fold_left (
          fun acc mv ->
            match mv with
            | Jump p -> if colr = White then acc||p=(x,-y) else acc||p=(-x,y)
            | Up -> acc || king_in_vec (x,y) (x',y') (x',y') (0,1) false
            | Right -> acc || king_in_vec (x,y) (x',y') (x',y') (1,0) false
            | DiagR -> acc || king_in_vec (x,y) (x',y') (x',y') (1,1) false
            | DiagL -> acc || king_in_vec (x,y) (x',y') (x',y') (-1,1) false
            | PawnMov b ->
              begin
                if colr = White then acc || pking (x,y) (x',y') (-1)
                else acc || pking (x',y') (x,y) 1
              end
            | KingMov b -> acc || kking (x,y) (x',y')
        ) false p
      end
  in
  let ncolor = if clr = Black then White else Black in
  check_helper kloc pcs ncolor

let val_move_lst (x,y) st =
  let pc = List.assoc (x,y) st.pieces in
  let val_lst lst = List.fold_left (
      fun acc (x',y') ->
        let new_pcs = ((x',y'),pc)::(List.remove_assoc (x,y) st.pieces) in
        if (st.color=White
            && not(in_check st.missing new_pcs st.color st.wking)) then
          (x',y')::acc
        else if (st.color=Black
            && not(in_check st.missing new_pcs st.color st.bking)) then
          (x',y')::acc
        else acc
    ) [] lst
  in
  let check_pos (a,b) acc =
    if (a > 11 || a < 0 || b > 11 || b < 0) then acc
    else if List.mem (a,b) st.missing then acc
    else if List.mem_assoc (a,b) st.pieces then
      if (List.assoc (a,b) st.pieces).pcolor = pc.pcolor then acc
      else (a,b)::acc
    else (a,b)::acc
  in
  let rec movs_of_vec (xs,ys) (vx,vy) inv (acc:'a list) =
    if not inv then
      let nx = xs+vx in
      let ny = ys+vy in
      let new_acc = check_pos (nx,ny) acc in
      if new_acc = acc then movs_of_vec (x,y) (vx,vy) true new_acc
      else if List.mem_assoc (nx,ny) st.pieces then
        movs_of_vec (x,y) (vx,vy) true new_acc
      else movs_of_vec (nx,ny) (vx,vy) false new_acc
    else
      let nx = -1*(xs+vx) in
      let ny = -1*(ys+vy) in
      let new_acc = check_pos (nx,ny) acc in
      if new_acc = acc then acc
      else if List.mem_assoc (nx,ny) st.pieces then
        new_acc
      else movs_of_vec (nx,ny) (vx,vy) true new_acc
  in
  let check_fr dir =
    if (List.mem (x,y+dir) st.missing
        && not (List.mem_assoc (x,y+dir) st.pieces)) then
      [(x,y+dir)]
    else []
  in
  let check_diag (x',y') acc =
    if ((List.mem_assoc (x',y') st.pieces)
        && (List.assoc (x',y') st.pieces).pcolor <> pc.pcolor
        && x' <= 11 && x' >= 0) then (x',y')::acc
    else acc
  in
  let pawn_mov dir two =
    if y+dir <= 11 || y+dir >= 0 then
      if two = false then
        let fr = check_fr dir in
        let lf = check_diag (x-1,y+dir) fr in
        check_diag (x+1,y+dir) lf
      else
        let fr = check_fr dir in
        let fr2 =
          match fr with
          | [] -> []
          | h::t -> h::(check_fr (dir*2))
        in
        let lf = check_diag (x-1,y+dir) fr2 in
        check_diag (x+1,y+dir) lf
    else failwith "Impossible"
  in
  let rook_castle dir =
    if List.mem_assoc (x+dir*3,y) st.pieces then
      let pc1 = List.assoc (x+dir*3,y) st.pieces in
      if pc1.name = (Rook true) && pc1.pcolor = st.color then true
      else if List.mem_assoc (x+dir*4,y) st.pieces then
        let pc2 = List.assoc (x+dir*4,y) st.pieces in
        if pc2.name = (Rook true) && pc2.pcolor = st.color then true
        else false
      else false
    else if List.mem_assoc (x+dir*4,y) st.pieces then
      let pc2 = List.assoc (x+dir*4,y) st.pieces in
      if pc2.name = (Rook true) && pc2.pcolor = st.color then true
      else false
    else false
  in
  let king_move castle =
    let basic_mov = ([] |> check_pos (x-1,y) |>
                    check_pos (x-1,y+1) |>
                    check_pos (x-1,y-1) |>
                    check_pos (x,y+1) |>
                    check_pos (x,y-1) |>
                    check_pos (x+1,y-1) |>
                    check_pos (x+1,y+1) |>
                    check_pos (x+1,y))
    in
    if castle = false then
      val_lst basic_mov
    else
      let new_lst = val_lst basic_mov in
      let left = List.mem (x-1,y) new_lst in
      let right = List.mem (x+1,y) new_lst in
      if left && right && rook_castle (-1) && rook_castle (1) then
        (val_lst [(x-2,y);(x+2,y)])@new_lst
      else if left && rook_castle (-1) then (val_lst [(x-2,y)])@new_lst
      else if right && rook_castle (1) then (val_lst [(x+2,y)])@new_lst
      else new_lst
  in
  List.fold_left (
    fun acc mv ->
      match mv with
      | Jump (x',y') ->
        (if pc.pcolor = White then val_lst (check_pos (x+x',-1*(y+y')) acc)
         else val_lst (check_pos (-1*(x+x'),y+y') acc))
      | Up -> val_lst (movs_of_vec (x,y) (0,-1) false acc)
      | Right -> val_lst (movs_of_vec (x,y) (1,0) false acc)
      | DiagR -> val_lst (movs_of_vec (x,y) (1,1) false acc)
      | DiagL -> val_lst (movs_of_vec (x,y) (-1,1) false acc)
      | PawnMov b ->
        if pc.pcolor = White then val_lst (pawn_mov (-1) b)
        else val_lst (pawn_mov 1 b)
      | KingMov b -> king_move b
  ) [] pc.pattern

let do' cmd st =
  let get_score name =
    match name with
    | Pawn -> 1
    | Rook _ -> 5
    | Knight | Bishop -> 3
    | Queen -> 8
    | King -> failwith "Impossible"
    | Custom _ -> 8
  in
  let update_score cap score =
    match (score,cap.pcolor) with
    | ((w,b),Black) -> (w,(get_score cap.name)+b)
    | ((w,b),White) -> ((get_score cap.name)+w,b)
  in
  let color_in_check clr pcs =
    let kloc = if clr = Black then st.bking else st.wking in
    if in_check st.missing pcs clr kloc then Some clr else None
  in
  let color_in_checkmate clr pcs =
    failwith "Unimplemented"
  in
  let mv (xi,yi) (xf,yf) =
    let pc = List.assoc (xi,yi) st.pieces in
    let ncolor = if st.color = Black then White else Black in
    if pc.name <> Pawn then
      if List.mem_assoc (xf,yf) st.pieces then
        let cap = List.assoc (xf,yf) st.pieces in
        let pc_lst = ((xf,yf),pc)::(st.pieces |>
          List.remove_assoc (xi,yi) |> List.remove_assoc (xf,yf)) in
        let cap_lst = cap::st.captured in
        if pc.name = King && st.color = Black then
          {
            missing = st.missing;
            pieces = pc_lst;
            captured = cap_lst;
            color = ncolor;
            promote = None;
            trow = st.trow;
            brow = st.brow;
            turn = st.turn + 1;
            score = update_score cap st.score;
            wking = st.wking;
            bking = (xf,yf);
            check = color_in_check ncolor pc_lst;
            checkmate = color_in_checkmate ncolor pc_lst
          }
        else if pc.name = King && st.color = White then
          {
            missing = st.missing;
            pieces = pc_lst;
            captured = cap_lst;
            color = ncolor;
            promote = None;
            trow = st.trow;
            brow = st.brow;
            turn = st.turn + 1;
            score = update_score cap st.score;
            wking = (xf,yf);
            bking = st.bking;
            check = color_in_check ncolor pc_lst;
            checkmate = color_in_checkmate ncolor pc_lst
          }
        else
          {
            missing = st.missing;
            pieces = pc_lst;
            captured = cap_lst;
            color = ncolor;
            promote = None;
            trow = st.trow;
            brow = st.brow;
            turn = st.turn + 1;
            score = update_score cap st.score;
            wking = st.wking;
            bking = st.bking;
            check = color_in_check ncolor pc_lst;
            checkmate = color_in_checkmate ncolor pc_lst
          }
      else
        let pc_lst = ((xf,yf),pc)::(st.pieces|>List.remove_assoc (xi,yi)) in
        if pc.name = King && st.color = Black then
          {
            missing = st.missing;
            pieces = pc_lst;
            captured = st.captured;
            color = ncolor;
            promote = None;
            trow = st.trow;
            brow = st.brow;
            turn = st.turn + 1;
            score = st.score;
            wking = st.wking;
            bking = (xf,yf);
            check = color_in_check ncolor st.pieces;
            checkmate = color_in_checkmate ncolor st.pieces
          }
        else if pc.name = King && st.color = White then
          {
            missing = st.missing;
            pieces = pc_lst;
            captured = st.captured;
            color = ncolor;
            promote = None;
            trow = st.trow;
            brow = st.brow;
            turn = st.turn + 1;
            score = st.score;
            wking = (xf,yf);
            bking = st.bking;
            check = color_in_check ncolor st.pieces;
            checkmate = color_in_checkmate ncolor st.pieces
          }
        else
          {
            missing = st.missing;
            pieces = pc_lst;
            captured = st.captured;
            color = ncolor;
            promote = None;
            trow = st.trow;
            brow = st.brow;
            turn = st.turn + 1;
            score = st.score;
            wking = st.wking;
            bking = st.bking;
            check = color_in_check ncolor pc_lst;
            checkmate = color_in_checkmate ncolor pc_lst
          }
    else if ((yf = st.trow && st.color = White)
             || (yf = st.brow && st.color = Black)) then
      if List.mem_assoc (xf,yf) st.pieces then
        let cap = List.assoc (xf,yf) st.pieces in
        let pc_lst = ((xf,yf),pc)::(st.pieces |>
                                    List.remove_assoc (xi,yi) |> List.remove_assoc (xf,yf)) in
        let cap_lst = cap::st.captured in
        {
          missing = st.missing;
          pieces = pc_lst;
          captured = cap_lst;
          color = st.color;
          promote = Some (xf,yf);
          trow = st.trow;
          brow = st.brow;
          turn = st.turn + 1;
          score = update_score cap st.score;
          wking = st.wking;
          bking = st.bking;
          check = color_in_check ncolor pc_lst;
          checkmate = color_in_checkmate ncolor pc_lst
        }
      else
        let pc_lst = ((xf,yf),pc)::(st.pieces|>List.remove_assoc (xi,yi)) in
        {
          missing = st.missing;
          pieces = pc_lst;
          captured = st.captured;
          color = st.color;
          promote = Some (xf,yf);
          trow = st.trow;
          brow = st.brow;
          turn = st.turn + 1;
          score = st.score;
          wking = st.wking;
          bking = st.bking;
          check = color_in_check ncolor pc_lst;
          checkmate = color_in_checkmate ncolor pc_lst
        }
    else
      if List.mem_assoc (xf,yf) st.pieces then
        let cap = List.assoc (xf,yf) st.pieces in
        let pc_lst = ((xf,yf),pc)::(st.pieces |>
                                    List.remove_assoc (xi,yi) |> List.remove_assoc (xf,yf)) in
        let cap_lst = cap::st.captured in
        {
          missing = st.missing;
          pieces = pc_lst;
          captured = cap_lst;
          color = ncolor;
          promote = None;
          trow = st.trow;
          brow = st.brow;
          turn = st.turn + 1;
          score = update_score cap st.score;
          wking = st.wking;
          bking = st.bking;
          check = color_in_check ncolor pc_lst;
          checkmate = color_in_checkmate ncolor pc_lst
        }
      else
        let pc_lst = ((xf,yf),pc)::(st.pieces|>List.remove_assoc (xi,yi)) in
        {
          missing = st.missing;
          pieces = pc_lst;
          captured = st.captured;
          color = ncolor;
          promote = None;
          trow = st.trow;
          brow = st.brow;
          turn = st.turn + 1;
          score = st.score;
          wking = st.wking;
          bking = st.bking;
          check = color_in_check ncolor pc_lst;
          checkmate = color_in_checkmate ncolor pc_lst
        }
  in
  let promote pc =
    let pos = match st.promote with Some p->p |None -> failwith "Impossible" in
    let ncolor = if st.color = Black then White else Black in
    let pc_lst = (pos,pc)::(List.remove_assoc pos st.pieces) in
    {
      missing = st.missing;
      pieces = pc_lst;
      captured = st.captured;
      color = ncolor;
      promote = None;
      trow = st.trow;
      brow = st.brow;
      turn = st.turn;
      score = st.score;
      wking = st.wking;
      bking = st.bking;
      check = color_in_check ncolor pc_lst;
      checkmate = color_in_checkmate ncolor pc_lst
    }
  in
  match cmd with
  | Move (init,fin) -> mv init fin
  | Promotion pc -> promote pc
  | Quit -> failwith "Quitting"
  | Invalid -> failwith "Invalid"
