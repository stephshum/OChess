open Command
open Models
open Yojson.Basic.Util

type state = {
  missing: position list;
  pieces: (name * (move list)) list;
  pc_loc: (position * piece) list;
  captured: piece list;
  color: color;
  promote: position option;
  trow: int;
  brow: int;
  turn: int;
  score: (int*int); (* left is White score, right is Black score *)
  wking: (int*int);
  bking: (int*int);
  powvalid: (position * power) list;
  check: color option;
  checkmate: color option
}

let init_state j =
  let int_tuple_of_json j =
    let higher1 str a b c d op1 op2=
      let ff = String.get str a in
      let fs = String.get str b in
      let sf = String.get str c in
      let ss = String.get str d in
      let cff = Char.code ff in
      let cfs = Char.code fs in
      let csf = Char.code sf in
      let css = Char.code ss in
      let intff = cff - 48 in
      let intfs = cfs - 48 in
      let intsf = csf - 48 in
      let intss = css - 48 in
      (op1 (intff*10 + intfs), op2(intsf*10 + intss))
    in
    let higher2 str a b op1 op2=
      let f = String.get str a in
      let s = String.get str b in
      let cf = Char.code f in
      let cs = Char.code s in
      let intf = cf - 48 in
      let ints = cs - 48 in
      (op1 intf, op2 ints)
    in
    let higher3 str a b c op1 op2=
      let f = String.get str a in
      let sf = String.get str b in
      let ss = String.get str c in
      let cf = Char.code f in
      let csf = Char.code sf in
      let css = Char.code ss in
      let intf = cf - 48 in
      let intsf = csf - 48 in
      let intss = css - 48 in
      (op1 intf, op2 (intsf*10 + intss))
    in
    let higher4 str a b c op1 op2 =
      let ff = String.get str a in
      let fs = String.get str b in
      let s = String.get str c in
      let cff = Char.code ff in
      let cfs = Char.code fs in
      let cs = Char.code s in
      let intff = cff - 48 in
      let intfs = cfs - 48 in
      let ints = cs - 48 in
      (op1 (intff*10 + intfs), op2 ints)
    in
    let str = j |> to_string in
    let len = str |> String.length in
    let find_neg1 = String.index_opt str '-' in
    let find_neg2 = String.rindex_opt str '-' in
    match (find_neg1, find_neg2) with
    | (Some n, Some i) -> begin
        if (n <> i) then begin
            if len = 9
            then
              higher1 str 2 3 6 7 (~-) (~-)
            else if len = 7
            then
              higher2 str 2 5 (~-) (~-)
            else if ((String.get str 3) = ',')
            then
              higher3 str 2 5 6 (~-) (~-)
            else
              higher4 str 2 3 6 (~-) (~-)
          end
          else if (n = 1) then begin
            if len = 8
            then
              higher1 str 2 3 5 6 (~-) (~+)
            else if len = 6
            then
              higher2 str 2 4 (~-) (~+)
            else if ((String.get str 3) = ',')
            then
              higher3 str 2 4 5 (~-) (~+)
            else
              higher4 str 2 3 5  (~-) (~+)
          end
          else begin
          if len = 8
          then
            higher1 str 1 2 5 6 (~+) (~-)
          else if len = 6
          then
            higher2 str 1 4 (~+) (~-)
          else if ((String.get str 2) = ',')
          then
            higher3 str 1 4 5 (~+) (~-)
          else
            higher4 str 1 2 5 (~+) (~-)
        end
    end
    | (None, None) ->
      if len = 7
      then
        higher1 str 1 2 4 5 (~+) (~+)
      else if len = 5
      then
        higher2 str 1 3 (~+) (~+)
      else if ((String.get str 2) = ',')
      then
        higher3 str 1 3 4 (~+) (~+)
      else
        higher4 str 1 2 4 (~+) (~+)
    | _ -> failwith "oops"
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
  let piece_of_json j =
  {
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
  let powerup_used1 =
    let num = Random.int 6 in
    match num with
    | 0 -> RaisetheDead
    | 1 -> Elimination
    | 2 -> NoJumpers
    | 3 -> SecondChance
    | 4 -> Clone
    | 5 -> MindControl
    | 6 -> CultMurder
    | _ -> failwith "oops"
  in
  let powerup_used2 =
    let num = Random.int 6 in
    match num with
    | 0 -> RaisetheDead
    | 1 -> Elimination
    | 2 -> NoJumpers
    | 3 -> SecondChance
    | 4 -> Clone
    | 5 -> MindControl
    | 6 -> CultMurder
    | _ -> failwith "oops"
  in
  let rcd =
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
      powvalid = [((1,4), powerup_used1); ((1,7), powerup_used1);
                  ((10,7), powerup_used2); ((10,4), powerup_used2)];
      check = j |> member "check" |> check_of_json;
      checkmate = j |> member "promote" |> checkmate_of_json
    }
  in
  rcd

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
                | DiagL -> acc'|| king_in_vec (x,y) (x',y') (x',y') (-1,1) false
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
      {st with checkmate = Some st.color
      }
    else
      st
  in
  let check_for_power (xf, yf) st =
    List.assoc_opt (xf, yf) st.powvalid
  in
  let pow0 (x,y) st =
    let higherpow l c =
      let poten = l in
      let predic p = List.mem p st.missing in
      let two_list = List.partition predic poten in
      let (_, not_void) = two_list in
      let jus_pos = List.map fst st.pc_loc in
      let predic2 p = List.mem p jus_pos in
      let two_list2 = List.partition predic2 not_void in
      let (_, valid) = two_list2 in
      let new_pawns = List.map (fun x -> (x, {name = Pawn true; pcolor = c})) valid in
      {st with pc_loc = new_pawns@st.pc_loc;
               powvalid = List.remove_assoc (x,y) st.powvalid}
    in
    if (st.color = White) then
      higherpow [(x,y-1); (x+1, y-1); (x-1, y-1)] White
    else
      higherpow [(x,y+1); (x+1, y+1); (x-1, y+1)] Black
  in
  let pow1 (x, y) st =
    let predic p = (fst (fst p) = x && (snd p).name <> King true
                    && (snd p).name <> King false) in
    let two_list = List.partition predic st.pc_loc in
    let (captured, updated) = two_list in
    let only_pieces = List.map snd captured in
    {st with pc_loc = updated;
             captured = only_pieces@st.captured;
             powvalid = List.remove_assoc (x,y) st.powvalid}
  in
  let pow2 (x,y) st =
    let predic1 (p: (name * (move list))) =
      let lst = snd p in
      List.fold_left (fun acc m -> match m with
          | Jump _ -> true
          | _ -> acc) false lst
    in
    let two_list1 = List.partition predic1 st.pieces in
    let (kill, leave) = two_list1 in
    let bad_names = List.map fst kill in
    let predic2 p =
      let pz = snd p in
      List.mem (pz.name) bad_names in
    let two_list2 = List.partition predic2 st.pc_loc in
    let (captured, updated) = two_list2 in
    let only_pieces = List.map snd captured in
    {st with pc_loc = updated;
             captured = only_pieces@st.captured;
             powvalid = List.remove_assoc (x,y) st.powvalid}
  in
  let pow3 (x, y) st =
    let higherpow c =
      let my_pieces = List.filter (fun x -> (x.pcolor = c)) st.captured in
      if my_pieces = [] then
         {st with powvalid = List.remove_assoc (x,y) st.powvalid}
      else
      let my_array = Array.of_list my_pieces in
      let my_len = Array.length my_array in
      let ran = Random.int (my_len) in
      let chosen = Array.get my_array ran in
      let rec rem lst acc a =
        match lst with
        | x::xs -> if x = a then acc@xs else rem xs (x::acc) a
        | [] -> acc
      in
      let new_cap = rem st.captured [] chosen in
      let ran_spot = (Random.int 12) in
      let new_place = (ran_spot, y) in
      if (List.mem_assoc new_place st.pc_loc) ||
         (List.mem new_place st.missing) then
         {st with powvalid = List.remove_assoc (x,y) st.powvalid}
      else
        {st with pc_loc = (new_place, chosen)::(st.pc_loc);
                 captured = new_cap;
                 powvalid = List.remove_assoc (x,y) st.powvalid}
    in
    if st.color = White then
      higherpow White
    else
      higherpow Black
  in
  let pow4 (x,y) st =
    let higherpow c =
      let my_pieces = List.filter (fun x ->
          let p = snd x in (p.pcolor = c) && p.name <> King true
                           && p.name <> King false) st.pc_loc in
      if my_pieces = [] then
         {st with powvalid = List.remove_assoc (x,y) st.powvalid}
      else
      let my_array = Array.of_list my_pieces in
      let my_len = Array.length my_array in
      let ran = Random.int my_len in
      let chosen = Array.get my_array ran in
      let ran_spot = Random.int 12 in
      let new_place = (ran_spot, y) in
      if (List.mem_assoc new_place st.pc_loc) ||
         (List.mem new_place st.missing) then
         {st with powvalid = List.remove_assoc (x,y) st.powvalid}
      else
        {st with pc_loc = (new_place, snd chosen)::(st.pc_loc);
                 powvalid = List.remove_assoc (x,y) st.powvalid}
    in
    if st.color = White then
      higherpow White
    else
      higherpow Black
  in
  let pow5 (x,y) st =
    let higherpow nc c=
      let my_pieces = List.filter (fun x ->
          let p = snd x in (p.pcolor = nc) && p.name <> King true
                           && p.name <> King false) st.pc_loc in
      if my_pieces = [] then
         {st with powvalid = List.remove_assoc (x,y) st.powvalid}
      else
      let my_array = Array.of_list my_pieces in
      let my_len = Array.length my_array in
      let ran = Random.int my_len in
      let chosen = Array.get my_array ran in
      let changed_piece = {(snd chosen) with pcolor = c} in
      let ran_spot = Random.int 12 in
      let new_place = (ran_spot, y) in
      if (List.mem_assoc new_place st.pc_loc) ||
         (List.mem new_place st.missing) then
         {st with powvalid = List.remove_assoc (x,y) st.powvalid}
      else
        {st with pc_loc = (new_place, changed_piece)::(st.pc_loc);
                 powvalid = List.remove_assoc (x,y) st.powvalid}
    in
    if st.color = White then
      higherpow Black White
    else
      higherpow White Black
  in
  let pow6 (x, y) st =
    let surrounding_tent = [(x-1,y+1); (x+1,y-1);
                            (x+1,y);(x,y+1);(x+1,y+1);
                            (x-1,y);(x,y-1);(x-1,y-1)] in
    let surrounding =
      if (st.wking =(x, y) || st.bking=(x, y)) then
        surrounding_tent
      else
        (x,y)::surrounding_tent
    in
    let predic p = List.mem (fst p) surrounding in
    let two_list = List.partition predic st.pc_loc in
    let (captured, updated) = two_list in
    let only_pieces = List.map snd captured in
    {st with pc_loc = updated;
             captured = only_pieces@st.captured;
             powvalid = List.remove_assoc (x,y) st.powvalid}
  in
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
     in
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
        let pre_state = {st with pc_loc = pc_lst;
                                 captured = cap_lst;
                                 promote = None;
                                 turn = st.turn + 1;
                                 score = update_score cap st.score;
                                 checkmate = None
                        }
        in
        let st' =
          if (pc.name = King true || pc.name = King false) &&
             st.color = Black then
            {pre_state with bking = (xf,yf)}
          else if (pc.name = King true || pc.name = King false)
               && st.color = White then
            {pre_state with wking = (xf,yf)}
          else
            pre_state
        in
        let pow = check_for_power (xf, yf) st' in
        let pow_state = use_power (xf, yf) pow st' in
        let truth  = in_check pow_state.missing pow_state.pieces pow_state.pc_loc ncolor kloc in
        let fin_state = {pow_state with check = color_in_check ncolor truth;
                                        color = ncolor
                        } in
        color_in_checkmate truth fin_state
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
        let pre_st = {st with pc_loc = pc_lst;
                              promote = None;
                              turn = st.turn + 1;
                              checkmate = None
                     }
        in
        let st' = if (pc.name = King true || pc.name = King false)
                  && st.color = Black then
            {pre_st with bking = (xf,yf)}
          else if (pc.name = King true || pc.name = King false)
               && st.color = White then
            {pre_st with wking = (xf,yf)}
          else
            pre_st
        in
        let pow = check_for_power (xf, yf) st' in
        let pow_state = use_power (xf, yf) pow st' in
        let truth  = in_check pow_state.missing pow_state.pieces pow_state.pc_loc ncolor kloc in
        let fin_state = {pow_state with color = ncolor;
                                        check = color_in_check ncolor truth} in
        color_in_checkmate truth fin_state
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
        let st' = {st with pc_loc = pc_lst;
                           captured = cap_lst;
                           promote = Some (xf,yf);
                           turn = st.turn + 1;
                           score = update_score cap st.score;
                           checkmate = None
                  }
        in
        let pow = check_for_power (xf, yf) st' in
        let pow_state = use_power (xf, yf) pow st' in
        let truth  = in_check pow_state.missing pow_state.pieces pow_state.pc_loc ncolor kloc in
        let fin_state = {pow_state with check = color_in_check ncolor truth} in
        color_in_checkmate truth fin_state
      else
        let pc_lst = ((xf,yf),new_pawn)::(st.pc_loc|>List.remove_assoc (xi,yi)) in
        let kloc = if ncolor = Black then st.bking else st.wking in
        let st' = {st with pc_loc = pc_lst;
                           promote = Some (xf,yf);
                           turn = st.turn + 1;
                           checkmate = None
                  }
        in
        let pow = check_for_power (xf, yf) st' in
        let pow_state = use_power (xf, yf) pow st' in
        let truth  = in_check pow_state.missing pow_state.pieces pow_state.pc_loc ncolor kloc in
        let fin_state = {pow_state with check = color_in_check ncolor truth} in
        color_in_checkmate truth fin_state
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
        let st' = {st with pc_loc = pc_lst;
                           captured = cap_lst;
                           promote = None;
                           turn = st.turn + 1;
                           score = update_score cap st.score;
                           checkmate = None
                  }
        in
        let pow = check_for_power (xf, yf) st' in
        let pow_state = use_power (xf, yf) pow st' in
        let truth  = in_check pow_state.missing pow_state.pieces pow_state.pc_loc ncolor kloc in
        let fin_state = {pow_state with check = color_in_check ncolor truth;
                                        color = ncolor
                        } in
        color_in_checkmate truth fin_state
      else
        let pc_lst = ((xf,yf),new_pawn)::(st.pc_loc|>List.remove_assoc (xi,yi))
        in
        let kloc = if ncolor = Black then st.bking else st.wking in
        let st' = {st with pc_loc = pc_lst;
                           promote = None;
                           turn = st.turn + 1;
                           checkmate = None}
        in
        let pow = check_for_power (xf, yf) st' in
        let pow_state = use_power (xf, yf) pow st' in
        let truth  = in_check pow_state.missing pow_state.pieces pow_state.pc_loc ncolor kloc in
        let fin_state = {pow_state with check = color_in_check ncolor truth;
                                        color = ncolor
                        } in
        color_in_checkmate truth fin_state
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
    let st' = {st with pc_loc = pc_lst;
                       color = ncolor;
                       promote = None;
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
