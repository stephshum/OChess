open Yojson
open Printf
open Dom
open Command
open State
open Models

(* shorthand for Dom_html properties and objects *)
let window = Dom_html.window
let document = window##document
let handler = Dom_html.handler

(* [stage] is the type representing what stage the users are in while using the
 * application *)
type stage = Custom_piece | Custom_board | Play | Start

(* [current_stage] is the stage the users are in currently and starts initially
 * at Custom_board *)
let current_stage = ref Start

(* [current_state] is the current state of the board game *)
let current_state : State.state ref = ref {
  missing = [];
  pieces = [];
  pc_loc = [];
  captured = [];
  color = White;
  promote = None;
  trow = 0;
  brow = 0;
  turn = 0;
  score = (0,0); (* left is White score, right is Black score *)
  wking = (0,0);
  bking = (0,0);
  powvalid = [];
  check = None;
  checkmate = None
}

(* [active_squares] is the list of squares on the board that are not void
 * denoted by a pair with row number and column number *)
let active_squares : ((int * int) list) ref = ref
    [
      (0,0);(0,1);(0,2);(0,3);(0,4);(0,5);(0,6);(0,7);(0,8);(0,9);(0,10);(0,11);
      (1,0);(1,1);(1,2);(1,3);(1,4);(1,5);(1,6);(1,7);(1,8);(1,9);(1,10);(1,11);
      (2,0);(2,1);(2,2);(2,3);(2,4);(2,5);(2,6);(2,7);(2,8);(2,9);(2,10);(2,11);
      (3,0);(3,1);(3,2);(3,3);(3,4);(3,5);(3,6);(3,7);(3,8);(3,9);(3,10);(3,11);
      (4,0);(4,1);(4,2);(4,3);(4,4);(4,5);(4,6);(4,7);(4,8);(4,9);(4,10);(4,11);
      (5,0);(5,1);(5,2);(5,3);(5,4);(5,5);(5,6);(5,7);(5,8);(5,9);(5,10);(5,11);
      (6,0);(6,1);(6,2);(6,3);(6,4);(6,5);(6,6);(6,7);(6,8);(6,9);(6,10);(6,11);
      (7,0);(7,1);(7,2);(7,3);(7,4);(7,5);(7,6);(7,7);(7,8);(7,9);(7,10);(7,11);
      (8,0);(8,1);(8,2);(8,3);(8,4);(8,5);(8,6);(8,7);(8,8);(8,9);(8,10);(8,11);
      (9,0);(9,1);(9,2);(9,3);(9,4);(9,5);(9,6);(9,7);(9,8);(9,9);(9,10);(9,11);
      (10,0);(10,1);(10,2);(10,3);(10,4);(10,5);(10,6);(10,7);(10,8);(10,9);
      (10,10);(10,11);
      (11,0);(11,1);(11,2);(11,3);(11,4);(11,5);(11,6);(11,7);(11,8);(11,9);
      (11,10);(11,11)
    ]

(* [void_squares] is the list of squares on the board that are void *)
let void_squares : ((int * int) list) ref = ref []

(* [pic_positions] is the list of possible images for pieces *)
let pic_positions : (string list) ref = ref
    ["b_pawn";"w_pawn";"b_rook";"w_rook";"b_knight";"w_knight";
     "b_bishop";"w_bishop";"b_queen";"w_queen";"b_king";"w_king";
     "b_custom";"w_custom"]

(* [custom_moves] is a list of possible movements for the custom piece *)
let custom_moves : (string list) ref = ref []

(* [highlighted] is a list of highlighted squares and their initial colors *)
let highlighted : ((Dom_html.element Js.t) * (Js.js_string Js.t)) list ref =
  ref []

(* [init_pieces] is a list of the pieces placed on the board before a
 * game has begun denoted by its name and the row and column of its square *)
let init_pieces : (string * (int * int)) list ref = ref []

(* [chosen_piece] is the piece picked to be moved on the board during play *)
let chosen_piece : Dom_html.element Js.t option ref = ref None

(* [chosen_image] is the image of a piece picked to be moved on to the board
 * before a game has begun*)
let chosen_image : string ref = ref "none"

(* [get_element s] is the HTML element with id [s] *)
let get_element s =
  Js.Opt.get (document##getElementById (Js.string s)) (fun () -> assert false)

(* [get_button s] is the HTML button with id [s] *)
let get_button s =
  match Dom_html.tagged (get_element s) with
  | Dom_html.Button button -> button
  | _ -> raise (Failure "Not a button")

(* [get_square r c] is the HTML element for a square on the board at row [r]
 * and column [c] *)
let get_square r c =
  get_element ("T-" ^ (string_of_int r) ^ "," ^ (string_of_int c))

(* [get_row r l] is a list of HTML elements in row [r] *)
let rec get_row r c (l : Dom_html.element Js.t list) =
  if List.length l = 12 then (
    List.iter (fun x -> let b' = x##style##backgroundColor in
    highlighted := (x,b')::(!highlighted);
    if Js.to_string (x##style##backgroundImage) = "none" then
      x##style##backgroundColor <- (Js.string "#ffd456")) l)
  else (
    get_row r (c+1) ((get_square r c)::l))

(* [get_col r l] is a list of HTML elements in column [c] *)
let rec get_col r c (l : Dom_html.element Js.t list) =
  if List.length l = 12 then (
    List.iter (fun x -> let b' = x##style##backgroundColor in
    highlighted := (x,b')::(!highlighted);
    if Js.to_string (x##style##backgroundImage) = "none" then
      x##style##backgroundColor <- (Js.string "#ffd456")) l)
  else (
    get_col (r+1) c ((get_square r c)::l))

(* [get_up_diagL r l] is a list of HTML elements in the left up diagonal *)
let rec get_up_diagL r c l =
  if r >= 0 && c >= 0 then
    get_up_diagL (r-1) (c-1) ((get_square r c)::l)
  else l

(* [get_d_diagL r l] is a list of HTML elements in the left down diagonal *)
let rec get_d_diagL r c l =
  if r < 12 && c < 12 then
    get_d_diagL (r+1) (c+1) ((get_square r c)::l)
  else l

(* [get_up_diagR r l] is a list of HTML elements in the right up diagonal *)
let rec get_up_diagR r c l =
  if r >= 0 && c < 12 then
    get_up_diagR (r-1) (c+1) ((get_square r c)::l)
  else l

(* [get_d_diagR r l] is a list of HTML elements in the right down diagonal *)
let rec get_d_diagR r c l =
  if r < 12 && c >= 0 then
    get_d_diagR (r+1) (c-1) ((get_square r c)::l)
  else l

(* [get_diag l] highlights the squares in [l] *)
let get_diag l =
  List.iter (fun x -> let b' = x##style##backgroundColor in
  highlighted := (x,b')::(!highlighted);
  if Js.to_string (x##style##backgroundImage) = "none" then
    x##style##backgroundColor <- (Js.string "#ffd456")) l

(* [get_image u] is the name of the image with url [u] *)
let get_image u =
  String.(sub u 12 (length u - 18))

(* [make_void e] makes the square for element [e] a void color *)
let make_void e =
  e##style##backgroundColor <- (Js.string "transparent")

(* [get_moves e] is the list of moves a piece at HTML element [e] can make *)
let get_moves e =
  let i = e##style##backgroundImage |> Js.to_string |> get_image in
  match i with
  | "w_pawn" -> ["Pawn"]
  | "w_rook" -> ["Up"; "Right"]
  | "w_knight" -> ["(1,2)";"(-1,2)";"(-2,1)";"(-2,-1)";"(1,-2)";
    "(-1,-2)";"(2,1)";"(2,-1)"]
  | "w_bishop" -> ["DiagL";"DiagR"]
  | "w_queen" -> ["DiagL";"DiagR";"Up";"Right"]
  | "w_king" -> ["King"]
  | _ -> !custom_moves

(* [highlight_one m] highlights squares corresponding to move [m] *)
let highlight_one r c m =
  match m with
  | "Up" -> get_col 0 c []
  | "Right" -> get_row r 0 []
  | "DiagL" -> get_diag ((get_up_diagL r c []) @ (get_d_diagL r c []))
  | "DiagR" -> get_diag ((get_up_diagR r c []) @ (get_d_diagR r c []))
  | x -> let com = String.index x ',' in
    let r' = String.(sub x 1 (com-1)) |> int_of_string in
    let c' = String.(sub x (com+1) (length x - com - 2)) |> int_of_string in
    let e = get_square (r+r') (c+c') in
    highlighted := (e,e##style##backgroundColor)::!highlighted;
    if Js.to_string (e##style##backgroundImage) = "none" then
      e##style##backgroundColor <- (Js.string "#ffd456")

(* [highlight_moves r c] highlights squares the piece at ([r],[c]) can move to
 * to yellow with move list [m] *)
let rec highlight_moves r c m =
  if m <> ["King"] && m <> ["Pawn"] then
    match m with
    | [] -> ()
    | h::t -> (highlight_one r c h); highlight_moves r c t

(* [handle_square r c _] is the callback for a square on the board at row [r]
 * and column [c] *)
let handle_square r c _ =
  let sq = get_square r c in
  let m = (11-r, c) in
  begin
    match !current_stage with
    | Start -> ()
    | Custom_piece ->
      let d = (r-6,c-6) in
      let p =
        begin
          match d with
          | (-1,0) | (1,0)  -> get_col 0 c []; "Up"
          | (0,-1) | (0,1)  -> get_row r 0 []; "Right"
          | (1,1)  | (-1,-1) ->
            get_diag ((get_up_diagL r c []) @ (get_d_diagL r c [])); "DiagL"
          | (-1,1) | (1,-1)  ->
            get_diag ((get_up_diagR r c []) @ (get_d_diagR r c [])); "DiagR"
          | _ -> "(" ^ (string_of_int (r-6)) ^ "," ^ (string_of_int (c-6)) ^ ")"
        end in
      let b = sq##style##backgroundColor in
      custom_moves := p::(!custom_moves);
      highlighted := (sq,b)::(!highlighted);
      sq##style##backgroundColor <- (Js.string "#ffd456")
    | Custom_board ->
      if !chosen_image = "none" then (
        void_squares := m::(r,c)::(!void_squares);
        active_squares := List.filter (fun x -> x <> (r,c) && (x <> m))
          !active_squares;
        make_void sq;
        snd m |> get_square (fst m) |> make_void)
      else (
        if List.mem (r,c) !active_squares then
          let s = String.(sub !chosen_image 14 (length !chosen_image - 14)) in
          let o = get_square (fst m) (snd m) in
          sq##style##backgroundImage <- (Js.string !chosen_image);
          o##style##backgroundImage <- (Js.string ("url('images/b_" ^ s)))
    | Play ->
      begin
        match !chosen_piece with
        | None ->
          chosen_piece := Some sq;
          highlight_moves r c (get_moves sq)
        | Some x ->
          chosen_piece := None;
          List.iter (fun a -> (fst a)##style##backgroundColor <- snd a)
            !highlighted;
          if List.mem (r,c) !active_squares then (
            let i = x##style##backgroundImage in
            x##style##backgroundImage <- (Js.string "none");
            sq##style##backgroundImage <- i)
      end
  end;
  Js._false

(* [square_callbacks l] registers callbacks for clicks on active board squares
 * listed in [l] *)
let rec square_callbacks l =
  match l with
  | [] -> ()
  | (r,c)::t -> (get_square r c)##onclick <- handler (handle_square r c);
    square_callbacks t

(* [handle_pic h e _] is the callback for an image of a piece *)
let handle_pic h e _ =
  begin
    match !current_stage with
    | Custom_board ->
      (if !chosen_image <> "none" then (
          let n = get_image !chosen_image in
          (get_element n)##style##backgroundColor <- (Js.string "transparent")));
      chosen_image := "url('images/" ^ h ^ ".png')";
      e##style##backgroundColor <- (Js.string "#2d5475");
    | _ -> ()
  end;
  Js._false

(* [pic_callbacks l] registers callbacks for clicks on images of pieces *)
let rec pic_callbacks l =
  match l with
  | [] -> ()
  | h::t -> let e = (get_element h) in
    e##onclick <- handler (handle_pic h e);
    pic_callbacks t

(* [now_playing ()] disables the buttons to customize the board and pieces while
 * the stage is Play *)
let now_playing () =
  (get_button "custom_board")##disabled <- Js._true;
  (get_button "custom_piece")##disabled <- Js._true;
  ()

(* [enable_buttons ()] enables all buttons at the beginning of game setup *)
let enable_buttons () =
  (get_button "custom_board")##disabled <- Js._false;
  (get_button "custom_piece")##disabled <- Js._false;
  (get_button "play_game")##disabled <- Js._false;
  Js._false

(* [handle_piece _] is the callback for the customize piece button *)
let handle_makepiece _ =
  current_stage := Custom_piece;
  let c = get_square 6 6 in
  c##style##backgroundImage <- (Js.string "url('images/w_custom.png')");
  window##alert (Js.string "You are now customizing a new piece. Click adjacent
  squares for unranged movement and others for jumps.");
  Js._false

(* [handle_board _] is the callback for the customize board button *)
let handle_makeboard _ =
  current_stage := Custom_board;
  chosen_image := "none";
  let c = get_square 6 6 in
  c##style##backgroundImage <- (Js.string "none");
  List.iter (fun x -> (fst x)##style##backgroundColor <- snd x) !highlighted;
  highlighted := [];
  window##alert (Js.string "You are now customizing your board. Click to turn
  squares into voids, and finally move pieces on to the board.");
  Js._false

(* [pieces_string x s] is the string of pieces and their starting positions *)
let rec pieces_string x s =
  begin
    match x with
    | [] -> s
    | (n,(r,c))::t ->
      let p = if String.get n 0 = 'b' then "Black" else "White" in
      let w = get_image n in
      let x = String.(length w - 2 |> sub w 2 |> capitalize_ascii) in
      pieces_string t (s^"{\"piece\":{\"name\":\"" ^ x ^ "\",\"color\":\"" ^ p ^
      "\"},\"position\":\"(" ^ (string_of_int r) ^ "," ^ (string_of_int c) ^
      ")\"},")
  end

(* [create_json ()] creates a JSON string for the initial state of the game *)
let create_json () =
  let void x y = x ^ ",\"(" ^ (y |> fst |> string_of_int) ^
                 "," ^ (y |> snd |> string_of_int) ^ ")\"" in
  let a' = List.fold_left void "" !void_squares in
  let a = "{\"missing\":[" ^ String.(sub a' 1 (length a' - 1)) in
  let b =  "],\"pieces\": [{\"name\": \"Rook\",\"pattern\": [ \"Up\","^
    "\"Right\" ]},{\"name\": \"Knight\",\"pattern\": [ \"(1,2)\", "^
    "\"(-1,2)\", \"(-2,1)\", \"(-2,-1)\",\"(1,-2)\", \"(-1,-2)\", "^
    "\"(2,1)\", \"(2,-1)\" ]},{\"name\": \"Bishop\", \"pattern\": ["^
    " \"DiagL\", \"DiagR\" ]},{\"name\":\"Queen\", \"pattern\": [ "^
    "\"DiagL\", \"DiagR\", \"Up\", \"Right\" ]},{\"name\": \"King\","^
    " \"pattern\": [ \"King\" ]},{\"name\": \"Pawn\",\"pattern\": [ \"Pawn\" ]"
  in
  let c = "},{\"name\":\"custom\",\"pattern\":[" in
  let d' = List.fold_left (fun x y -> x ^ "\"" ^ y ^ "\",") "" !custom_moves in
  let d = String.(sub d' 0 (length d' - 1)) ^ "}],\"pc_loc\": [" in
  let e' = pieces_string !init_pieces "" in
  let e = String.(sub e' 0 (length e' - 1)) in
  let f = "],\n  \"captured\": [],\n  \"color\": \"White\",\"" in
  let g = "\"trow\":" ^ List.(hd !active_squares |> fst |> string_of_int)^"," in
  let h = "\"brow\":" ^ List.(length !active_squares - 1 |> nth !active_squares
    |> snd |> string_of_int) ^ "," in
  let i = "\"promote\": \"None\",\"turn\": 1,\"score\": \"(0,0)\"," in
  let j' = List.assoc "url('images/w_king.png')" !init_pieces in
  let j = "\"wking\":\"(" ^ (string_of_int (fst j')) ^ "," ^
    (string_of_int (snd j')) ^ ")\"," in
  let k' = List.assoc "url('images/b_king.png')" !init_pieces in
  let k = "\"bking\":\"(" ^ (string_of_int (fst k')) ^ "," ^
    (string_of_int (snd k')) ^ ")\"," in
  let l = " \"check\": \"None\", \"checkmate\": \"None\"}" in
  let json = a^b^c^d^e^f^g^h^i^j^k^l in
  Yojson.Basic.from_string json

(* [handle_play _] is the callback for the start game button *)
let handle_play _ =
  current_stage := Play;
  chosen_image := "none";
  now_playing ();
  current_state := State.init_state (create_json ());
  window##alert (Js.string "Start playing! You will not be able to customize
  the board or pieces further.");
  Js._false

let onload _ =
  (get_element "custom_piece")##onclick <- (handler handle_makepiece);
  (get_element "custom_board")##onclick <- (handler handle_makeboard);
  (get_element "play_game")##onclick <- (handler handle_play);
  square_callbacks !active_squares;
  pic_callbacks !pic_positions;
  Js._false
