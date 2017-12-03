open Command
open Models
open Yojson.Basic.Util

type state = {
  missing: position list;
  pieces: (name*(move list)) list;
  pc_loc: (position*(piece)) list;
  captured: piece list;
  color: color;
  promote: position option;
  trow: int;
  brow: int;
  turn: int;
  score: (int*int); (* left is White score, right is Black score *)
  wking: (int*int);
  bking: (int*int);
  powvalid: (position*power) list;
  check: color option;
  checkmate: color option
}

let init_state j =
  let re = Str.regexp "-*[0-9]+" in
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
    if s = "Pawn" then Pawn true
    else if s = "Rook" then Rook true
    else if s = "Knight" then Knight
    else if s = "Bishop" then Bishop
    else if s = "Queen" then Queen
    else if s = "King" then King true
    else Custom s
  in
  let pattern_of_json j =
    let s = j |> to_string in
    if s = "Up" then Up
    else if s = "Right" then Right
    else if s = "DiagR" then DiagR
    else if s = "DiagL" then DiagL
    else if s = "Pawn" then PawnMov
    else if s = "King" then KingMov
    else Jump (j |> int_tuple_of_json)
  in
  let piece_ref_of_json j =
    let name = j |> member "name" |> name_of_json in
    let pattern =
      j |> member "pattern" |> to_list |> List.map pattern_of_json
    in
    (name,pattern)
  in
  let piece_of_json j = {
    name = j |> member "name" |> name_of_json;
    pcolor = j |> member "color" |> color_of_json;
  }
  in
  let pc_loc_of_json j =
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
  let powerup_used =
    let num = Random.int 6 in
    match num with
    | 0 -> RaisetheDead
    | 1 -> Elimination
    | 2 -> NoJumpers
    | 3 -> SecondChance
    | 4 -> Clone
    | 5 -> MindControl
    | 6 -> CultMurder
  in
  {
    missing = j |> member "missing" |> to_list |> List.map int_tuple_of_json;
    pieces = j |> member "pieces" |> to_list |> List.map piece_ref_of_json;
    pc_loc = j |> member "pc_loc" |> to_list |> List.map pc_loc_of_json;
    captured = j |> member "captured" |> to_list |> List.map piece_of_json;
    color = j |> member "color" |> color_of_json;
    promote = None;
    trow = j |> member "trow" |> to_int;
    brow = j |> member "brow" |> to_int;
    turn = j |> member "turn" |> to_int;
    score = j |> member "score" |> int_tuple_of_json;
    wking = j |> member "wking" |> int_tuple_of_json;
    bking = j |> member "bking" |> int_tuple_of_json;
    powvalid = (((5,2), powerup_used); ((6,9), powerup_used));
    check = j |> member "check" |> check_of_json;
    checkmate = j |> member "promote" |> checkmate_of_json
  }

(* [in_check miss pcs clr kloc] true if [clr] king is in check
 * requires:
 *  [miss] is a valid list of missing board positions
 *  [pcs] is a valid list of piece names associated to their moves
 *  [pc_loc] is a valid list of pieces associated to their location
 *  [clr] is the color of the king to check
 *  [kloc] is the location of the king whose color is given by [clr] *)
let in_check miss pcs pc_loc clr kloc =
  let rec king_in_vec (kx,ky) (x,y) (x',y') (vx,vy) inv =
    if not inv then
      let nx = x'+vx in
      let ny = y'+vy in
      if (nx > 11 || nx < 0 || ny > 11 || ny < 0) then
        king_in_vec (kx,ky) (x,y) (x,y) (vx,vy) true
      else if List.mem (nx,ny) miss then
        king_in_vec (kx,ky) (x,y) (x,y) (vx,vy) true
      else if List.mem_assoc (nx,ny) pc_loc then
        if (nx,ny) = (kx,ky) then true
        else king_in_vec (kx,ky) (x,y) (x,y) (vx,vy) true
      else
        king_in_vec (kx,ky) (x,y) (nx,ny) (vx,vy) false
    else
      let nx = x'+(-1*vx) in
      let ny = y'+(-1*vy) in
      if (nx > 11 || nx < 0 || ny > 11 || ny < 0) then false
      else if List.mem (nx,ny) miss then false
      else if List.mem_assoc (nx,ny) pc_loc then
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
  let rec check_helper (x,y) pc_loc colr acc =
    match pc_loc with
    | [] -> acc
    | ((x',y'),{name=n;pcolor=colr2})::t ->
      begin
        if colr2 = colr then
          let p =
            match n with
            | Pawn _ -> List.assoc (Pawn true) pcs
            | King _ -> List.assoc (King true) pcs
            | Rook _ -> List.assoc (Rook true) pcs
            | _ -> List.assoc n pcs
          in
          check_helper (x,y) t colr (acc || List.fold_left (
              fun acc' mv ->
                match mv with
                | Jump (dv,dy) ->
                  begin
                    if colr = White then
                      acc'||(x',-dy)=(x,y)
                    else acc'||(-x',dy)=(x,y)
                  end
                | Up -> acc' || king_in_vec (x,y) (x',y') (x',y') (0,1) false
                | Right -> acc' || king_in_vec (x,y) (x',y') (x',y') (1,0) false
                | DiagR -> acc' || king_in_vec (x,y) (x',y') (x',y') (1,1) false
                | DiagL -> acc' || king_in_vec (x,y) (x',y') (x',y') (-1,1) false
                | PawnMov ->
                  begin
                    if colr = White then acc' || pking (x,y) (x',y') (-1)
                    else acc' || pking (x,y) (x',y') 1
                  end
                | KingMov -> acc' || kking (x,y) (x',y')
            ) false p)
        else
          check_helper (x,y) t colr acc
      end
  in
  let ncolor = if clr = Black then White else Black in
  check_helper kloc pc_loc ncolor false

let val_move_lst (x,y) st =
  let pc = List.assoc (x,y) st.pc_loc in
  let val_lst lst = List.fold_left (
      fun acc (x',y') ->
        let new_pcs = ((x',y'),pc)::(List.remove_assoc (x,y) st.pc_loc) in
        if pc.name = King true || pc.name = King false then
          if (st.color=White &&
              not(in_check st.missing st.pieces new_pcs st.color (x',y'))) then
            (x',y')::acc
          else if (st.color=Black &&
              not(in_check st.missing st.pieces new_pcs st.color (x',y'))) then
            (x',y')::acc
          else acc
        else
          if (st.color=White &&
              not(in_check st.missing st.pieces new_pcs st.color st.wking)) then
            (x',y')::acc
          else if (st.color=Black &&
              not(in_check st.missing st.pieces new_pcs st.color st.bking)) then
            (x',y')::acc
          else acc
    ) [] lst
  in
  let check_pos (a,b) acc =
    if (a > 11 || a < 0 || b > 11 || b < 0) then acc
    else if List.mem (a,b) st.missing then acc
    else if List.mem_assoc (a,b) st.pc_loc then
      if (List.assoc (a,b) st.pc_loc).pcolor = pc.pcolor then acc
      else (a,b)::acc
    else (a,b)::acc
  in
  let rec movs_of_vec (xs,ys) (vx,vy) inv (acc:'a list) =
    if not inv then
      let nx = xs+vx in
      let ny = ys+vy in
      let new_acc = check_pos (nx,ny) acc in
      if new_acc = acc then movs_of_vec (x,y) (vx,vy) true new_acc
      else if List.mem_assoc (nx,ny) st.pc_loc then
        movs_of_vec (x,y) (vx,vy) true new_acc
      else movs_of_vec (nx,ny) (vx,vy) false new_acc
    else
      let nx = xs+(-1*vx) in
      let ny = ys+(-1*vy) in
      let new_acc = check_pos (nx,ny) acc in
      if new_acc = acc then acc
      else if List.mem_assoc (nx,ny) st.pc_loc then
        new_acc
      else movs_of_vec (nx,ny) (vx,vy) true new_acc
  in
  let check_fr dir =
    if (not (List.mem (x,y+dir) st.missing)
        && not (List.mem_assoc (x,y+dir) st.pc_loc)) then
      [(x,y+dir)]
    else []
  in
  let check_diag (x',y') acc =
    if ((List.mem_assoc (x',y') st.pc_loc)
        && (List.assoc (x',y') st.pc_loc).pcolor <> pc.pcolor
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
    if List.mem_assoc (x+dir*3,y) st.pc_loc then
      let pc1 = List.assoc (x+dir*3,y) st.pc_loc in
      if pc1.name = (Rook true) && pc1.pcolor = st.color then true
      else false
    else if List.mem_assoc (x+dir*4,y) st.pc_loc then
      let pc2 = List.assoc (x+dir*4,y) st.pc_loc in
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
      let left2 = not (List.mem_assoc (x-2,y) st.pc_loc) in
      let right = List.mem (x+1,y) new_lst in
      let right2 = not (List.mem_assoc (x+2,y) st.pc_loc) in
      if left&&right&&left2&&right2&&rook_castle (-1) && rook_castle (1) then
        (val_lst [(x-2,y);(x+2,y)])@new_lst
      else if left && left2 && rook_castle (-1) then (val_lst [(x-2,y)])@new_lst
      else if right &&right2 && rook_castle (1) then (val_lst [(x+2,y)])@new_lst
      else new_lst
  in
  let truth_of_name name =
    match name with
    | Pawn b -> b
    | King b -> b
    | _ -> failwith "Impossible"
  in
  let p =
    match pc.name with
    | Pawn _ -> List.assoc (Pawn true) st.pieces
    | King _ -> List.assoc (King true) st.pieces
    | Rook _ -> List.assoc (Rook true) st.pieces
    | _ -> List.assoc pc.name st.pieces
  in
  List.fold_left (
    fun acc mv ->
      match mv with
      | Jump (x',y') ->
        (if pc.pcolor = White then val_lst (check_pos (x+x',y+(-1*y')) acc)
         else val_lst (check_pos (x+(-1*x'),y+y') acc))
      | Up -> val_lst (movs_of_vec (x,y) (0,-1) false acc)
      | Right -> val_lst (movs_of_vec (x,y) (1,0) false acc)
      | DiagR -> val_lst (movs_of_vec (x,y) (1,1) false acc)
      | DiagL -> val_lst (movs_of_vec (x,y) (-1,1) false acc)
      | PawnMov ->
        let b = truth_of_name pc.name in
        if pc.pcolor = White then val_lst (pawn_mov (-1) b)
        else val_lst (pawn_mov 1 b)
      | KingMov -> let b = truth_of_name pc.name in king_move b
  ) [] p

let do' cmd st =
  let get_score name =
    match name with
    | Pawn _ -> 1
    | Rook _ -> 5
    | Knight | Bishop -> 3
    | Queen -> 9
    | King _ -> failwith "Impossible"
    | Custom _ -> 7
  in
  let update_score cap score =
    match (score,cap.pcolor) with
    | ((w,b),White) -> (w,(get_score cap.name)+b)
    | ((w,b),Black) -> ((get_score cap.name)+w,b)
  in
  let color_in_check clr check =
    if check then Some clr else None
  in
  let mate_of_pc moves =
    match moves with
    | [] -> true
    | h::t -> false
  in
  let color_in_checkmate truth st =
    if (truth && List.fold_left (fun acc (pos,pc) ->
        if pc.pcolor = st.color then
          acc && mate_of_pc (val_move_lst pos st)
        else acc) true st.pc_loc) then
      {
        missing = st.missing;
        pieces = st.pieces;
        pc_loc = st.pc_loc;
        captured = st.captured;
        color = st.color;
        promote = st.promote;
        trow = st.trow;
        brow = st.brow;
        turn = st.turn;
        score = st.score;
        wking = st.wking;
        bking = st.bking;
        check = st.check;
        checkmate = Some st.color
      }
    else
     st
  in
  (* [check_for_power (xf, yf) st] will return [Some x] if there is a
   * powerup associated with the positions, None otherwise*)
  let check_for_power (xf, yf) st =
    List.assoc_opt (xf, yf) st.powvalid
  in
  (* [pow1 loc st] will change the state for the powerup Elimination
   * located at [loc] as in "powerup_ideas.txt"*)
  let pow1 (x,y) st =
    let predic p = (snd (fst p) = y) in
    let two_list = List.partition predic st.pc_loc in
    let (captured, updated) = two_list in
    let only_pieces = List.map fst captured in
    {st with pc_loc = updated_pieces;
             captured = only_pieces@st.captured}
  in
  (* [pow6 loc st] will change the state for the powerup CultMurder
   * located at [loc] as in "powerup_ideas.txt"*)
  let pow6 (x, y) st =
    let surrounding = [(x,y);(x-1,y+1); (x+1,y-1);
                       (x+1,y);(x,y+1);(x+1,y+1);
                       (x-1,y);(x,y-1);(x-1,y-1)] in
    let predic p = List.mem (fst p) surrounding in
    let two_list = List.partition predic st.pc_loc in
    let (captured, updated) = two_list in
    let only_pieces = List.map fst captured in
    {st with pc_loc = updated;
             captured = only_pieces@st.captured}
  (* [use_power pow st] will return an updated state with changes enacted
   * as described in "powerup_ideas.txt" if [pow] is [Some x]. If
   * [pow] is [None],st will remain the same
   * *)
  let use_power loc pow st =
    match pow with
    | Some x -> begin
        match x with
        | RaisetheDead -> pow0 loc st
        | Elimination -> pow1 loc st
        | NoJumpers -> pow2 loc st
        | SecondChance -> pow3 loc st
        | Clone -> pow4 loc st
        | MindControl -> pow5 loc st
        | CultMurder -> pow6 loc st
    end
    | None -> st
  let mv (xi,yi) (xf,yf) =
    let pc = List.assoc (xi,yi) st.pc_loc in
    let ncolor = if st.color = Black then White else Black in
    if pc.name <> Pawn true && pc.name <> Pawn false then
      if List.mem_assoc (xf,yf) st.pc_loc then
        let cap = List.assoc (xf,yf) st.pc_loc in
        let pc_lst =
          match pc.name with
          | King _ ->
            begin
              let new_king = {
                name = King false;
                pcolor = pc.pcolor
              }
              in
              ((xf,yf),new_king)::(st.pc_loc |>
                  List.remove_assoc (xi,yi) |> List.remove_assoc (xf,yf))
            end
          | Rook _ ->
            begin
            let new_rook = {
              name = Rook false;
              pcolor = pc.pcolor
            }
            in
            ((xf,yf),new_rook)::(st.pc_loc |>
                List.remove_assoc (xi,yi) |> List.remove_assoc (xf,yf))
            end
          | _ -> ((xf,yf),pc)::(st.pc_loc |>
              List.remove_assoc (xi,yi) |> List.remove_assoc (xf,yf))
        in
        let cap_lst = cap::st.captured in
        let kloc = if ncolor = Black then st.bking else st.wking in
        let truth  = in_check st.missing st.pieces pc_lst ncolor kloc in
        if (pc.name = King true || pc.name = King false) &&
        st.color = Black then
          let st' = {
              missing = st.missing;
              pieces = st.pieces;
              pc_loc = pc_lst;
              captured = cap_lst;
              color = ncolor;
              promote = None;
              trow = st.trow;
              brow = st.brow;
              turn = st.turn + 1;
              score = update_score cap st.score;
              wking = st.wking;
              bking = (xf,yf);
              check = color_in_check ncolor truth;
              checkmate = None
            }
          in
          color_in_checkmate truth st'
        else if (pc.name = King true || pc.name = King false)
        && st.color = White then
          let st' = {
              missing = st.missing;
              pieces = st.pieces;
              pc_loc = pc_lst;
              captured = cap_lst;
              color = ncolor;
              promote = None;
              trow = st.trow;
              brow = st.brow;
              turn = st.turn + 1;
              score = update_score cap st.score;
              wking = (xf,yf);
              bking = st.bking;
              check = color_in_check ncolor truth;
              checkmate = None
            }
          in
          color_in_checkmate truth st'
        else
          let st' = {
              missing = st.missing;
              pieces = st.pieces;
              pc_loc = pc_lst;
              captured = cap_lst;
              color = ncolor;
              promote = None;
              trow = st.trow;
              brow = st.brow;
              turn = st.turn + 1;
              score = update_score cap st.score;
              wking = st.wking;
              bking = st.bking;
              check = color_in_check ncolor truth;
              checkmate = None
            }
          in
          color_in_checkmate truth st'
      else
        let pc_lst =
          match pc.name with
          | King _ ->
            begin
              let new_king = {
                name = King false;
                pcolor = pc.pcolor
              }
              in
              if xf = xi-2 then
                if List.mem_assoc (xi-3,yi) st.pc_loc then
                  let rk = {
                    name = Rook false;
                    pcolor = pc.pcolor
                  }
                  in
                  ((xf,yf),new_king)::((xi-1,yi),rk)::(st.pc_loc|>
                        List.remove_assoc (xi,yi)|>List.remove_assoc (xi-3,yi))
                else
                  let rk = {
                    name = Rook false;
                    pcolor = pc.pcolor
                  }
                  in
                  ((xf,yf),new_king)::((xi-1,yi),rk)::(st.pc_loc|>
                        List.remove_assoc (xi,yi)|>List.remove_assoc (xi-4,yi))
              else if xf = xi+2 then
                if List.mem_assoc (xi+3,yi) st.pc_loc then
                  let rk = {
                    name = Rook false;
                    pcolor = pc.pcolor
                  }
                  in
                  ((xf,yf),new_king)::((xi+1,yi),rk)::(st.pc_loc|>
                        List.remove_assoc (xi,yi)|>List.remove_assoc (xi+3,yi))
                else
                  let rk = {
                    name = Rook false;
                    pcolor = pc.pcolor
                  }
                  in
                  ((xf,yf),new_king)::((xi+1,yi),rk)::(st.pc_loc|>
                        List.remove_assoc (xi,yi)|>List.remove_assoc (xi+4,yi))
              else
                ((xf,yf),new_king)::(st.pc_loc|>List.remove_assoc (xi,yi))
            end
          | Rook _ ->
            begin
            let new_rook = {
              name = Rook false;
              pcolor = pc.pcolor
            }
            in
            ((xf,yf),new_rook)::(st.pc_loc|>List.remove_assoc (xi,yi))
            end
          | _ -> ((xf,yf),pc)::(st.pc_loc|>List.remove_assoc (xi,yi))
        in
        let kloc = if ncolor = Black then st.bking else st.wking in
        let truth  = in_check st.missing st.pieces pc_lst ncolor kloc in
        if (pc.name = King true || pc.name = King false)
        && st.color = Black then
          let st' = {
              missing = st.missing;
              pieces = st.pieces;
              pc_loc = pc_lst;
              captured = st.captured;
              color = ncolor;
              promote = None;
              trow = st.trow;
              brow = st.brow;
              turn = st.turn + 1;
              score = st.score;
              wking = st.wking;
              bking = (xf,yf);
              check = color_in_check ncolor truth;
              checkmate = None
            }
          in
          color_in_checkmate truth st'
        else if (pc.name = King true || pc.name = King false)
          && st.color = White then
          let st' = {
              missing = st.missing;
              pieces = st.pieces;
              pc_loc = pc_lst;
              captured = st.captured;
              color = ncolor;
              promote = None;
              trow = st.trow;
              brow = st.brow;
              turn = st.turn + 1;
              score = st.score;
              wking = (xf,yf);
              bking = st.bking;
              check = color_in_check ncolor truth;
              checkmate = None
            }
          in
          color_in_checkmate truth st'
        else
          let st' = {
              missing = st.missing;
              pieces = st.pieces;
              pc_loc = pc_lst;
              captured = st.captured;
              color = ncolor;
              promote = None;
              trow = st.trow;
              brow = st.brow;
              turn = st.turn + 1;
              score = st.score;
              wking = st.wking;
              bking = st.bking;
              check = color_in_check ncolor truth;
              checkmate = None
            }
          in
          color_in_checkmate truth st'
    else if ((yf = st.trow && st.color = White)
             || (yf = st.brow && st.color = Black)) then
      let new_pawn = {
        name = Pawn false;
        pcolor = pc.pcolor
      }
      in
      if List.mem_assoc (xf,yf) st.pc_loc then
        let cap = List.assoc (xf,yf) st.pc_loc in
        let pc_lst = ((xf,yf),new_pawn)::(st.pc_loc |>
            List.remove_assoc (xi,yi) |> List.remove_assoc (xf,yf)) in
        let cap_lst = cap::st.captured in
        let kloc = if ncolor = Black then st.bking else st.wking in
        let truth  = in_check st.missing st.pieces pc_lst ncolor kloc in
        let st' = {
            missing = st.missing;
            pieces = st.pieces;
            pc_loc = pc_lst;
            captured = cap_lst;
            color = st.color;
            promote = Some (xf,yf);
            trow = st.trow;
            brow = st.brow;
            turn = st.turn + 1;
            score = update_score cap st.score;
            wking = st.wking;
            bking = st.bking;
            check = color_in_check ncolor truth;
            checkmate = None
          }
        in
        color_in_checkmate truth st'
      else
        let pc_lst = ((xf,yf),new_pawn)::(st.pc_loc|>List.remove_assoc (xi,yi)) in
        let kloc = if ncolor = Black then st.bking else st.wking in
        let truth  = in_check st.missing st.pieces pc_lst ncolor kloc in
        let st' = {
            missing = st.missing;
            pieces = st.pieces;
            pc_loc = pc_lst;
            captured = st.captured;
            color = st.color;
            promote = Some (xf,yf);
            trow = st.trow;
            brow = st.brow;
            turn = st.turn + 1;
            score = st.score;
            wking = st.wking;
            bking = st.bking;
            check = color_in_check ncolor truth;
            checkmate = None
          }
        in
        color_in_checkmate truth st'
    else
      let new_pawn = {
        name = Pawn false;
        pcolor = pc.pcolor
      }
      in
      if List.mem_assoc (xf,yf) st.pc_loc then
        let cap = List.assoc (xf,yf) st.pc_loc in
        let pc_lst = ((xf,yf),new_pawn)::(st.pc_loc |>
            List.remove_assoc (xi,yi) |> List.remove_assoc (xf,yf)) in
        let cap_lst = cap::st.captured in
        let kloc = if ncolor = Black then st.bking else st.wking in
        let truth  = in_check st.missing st.pieces pc_lst ncolor kloc in
        let st' = {
            missing = st.missing;
            pieces = st.pieces;
            pc_loc = pc_lst;
            captured = cap_lst;
            color = ncolor;
            promote = None;
            trow = st.trow;
            brow = st.brow;
            turn = st.turn + 1;
            score = update_score cap st.score;
            wking = st.wking;
            bking = st.bking;
            check = color_in_check ncolor truth;
            checkmate = None
          }
        in
        color_in_checkmate truth st'
      else
        let pc_lst = ((xf,yf),new_pawn)::(st.pc_loc|>List.remove_assoc (xi,yi))
        in
        let kloc = if ncolor = Black then st.bking else st.wking in
        let truth  = in_check st.missing st.pieces pc_lst ncolor kloc in
        let st' = {
            missing = st.missing;
            pieces = st.pieces;
            pc_loc = pc_lst;
            captured = st.captured;
            color = ncolor;
            promote = None;
            trow = st.trow;
            brow = st.brow;
            turn = st.turn + 1;
            score = st.score;
            wking = st.wking;
            bking = st.bking;
            check = color_in_check ncolor truth;
            checkmate = None
          }
        in
        color_in_checkmate truth st'
  in
  let pc_of_name name =
    {
      name = name;
      pcolor = st.color
    }
  in
  let promote name =
    let pc = pc_of_name name in
    let pos = match st.promote with Some p->p |None -> failwith "Impossible" in
    let ncolor = if st.color = Black then White else Black in
    let pc_lst = (pos,pc)::(List.remove_assoc pos st.pc_loc) in
    let kloc = if ncolor = Black then st.bking else st.wking in
    let truth  = in_check st.missing st.pieces pc_lst ncolor kloc in
    let st' = {
        missing = st.missing;
        pieces = st.pieces;
        pc_loc = pc_lst;
        captured = st.captured;
        color = ncolor;
        promote = None;
        trow = st.trow;
        brow = st.brow;
        turn = st.turn;
        score = st.score;
        wking = st.wking;
        bking = st.bking;
        check = color_in_check ncolor truth;
        checkmate = None
      }
    in
    color_in_checkmate truth st'
  in
  match cmd with
  | Move (init,fin) -> mv init fin
  | PreMove _ -> failwith "Premove"
  | Promotion name -> promote name
  | Quit -> failwith "Quitting"
  | Invalid -> failwith "Invalid"
