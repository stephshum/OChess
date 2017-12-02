open Yojson
open State
open Dom

(* shorthand for Dom_html properties *)
let window = Dom_html.window
let doc = window##document
let document = Dom_html.document
let handler = Dom_html.handler

(* [stage] is the type representing what stage the users are in while using the
 * application *)
type stage = Custom_board | Custom_piece | Play

(* [current_stage] is the stage the users are in currently and starts initially
 * at Custom_board *)
let current_stage = ref Custom_board

(* [active_squares] is the list of squares on the board that are not void
 * denoted by a pair with row number and column number *)
let active_squares : ((int * int) list) ref = ref []

(* [init_piece_pos] is a list of the pieces placed on the board before a
 * game has begun denoted by its name and the row and column of its square *)
let init_piece_pos : (string * (int * int)) list ref = ref []

(* [chosen_piece] is the piece picked to be moved on the board *)
let chosen_piece : Dom_html.element Js.t option ref = ref None

(* [get_element s] is the HTML element with id [s] *)
let get_element s =
  Js.Opt.get (document##getElementById (Js.string s)) (fun () -> assert false)

(* [get_button s] is the HTML button with id [s] *)
let get_button s =
  match Dom_html.tagged (get_element s) with
  | Dom_html.Button button -> button
  | _ -> raise (Failure "Not a button")

(* [now_playing ()] disables the buttons to customize the board and pieces while
 * the stage is Play *)
let now_playing () =
  (get_button "custom_board")##disabled <- Js._true;
  (get_button "custom_piece")##disabled <- Js._true;
  Js._false

(* [enable_buttons ()] enables all buttons at the beginning of game setup *)
let enable_buttons () =
  (get_button "custom_board")##disabled <- Js._false;
  (get_button "custom_piece")##disabled <- Js._false;
  (get_button "play_game")##disabled <- Js._false;
  Js._false

(* [get_square r c] is the HTML element for a square on the board at row [r]
 * and column [c] *)
let get_square r c =
  get_element ("T-" ^ (string_of_int r) ^ "," ^ (string_of_int c))

(* [handle_void r c] is the callback for a square on the board at row [r]
 * and column [c] when customizing the board *)
let handle_void r c =
  let square = get_square r c in
  square##style##color <- (Js.string "#24425b")

(* [get_image r c] is the image of the piece at element [e] *)
let get_image e =
  failwith "Unimpl"

(* [handle_square r c] is the callback for a square on the board at row [r]
 * and column [c] *)
let handle_square r c _ =
  begin
    match !chosen_piece with
    | None -> chosen_piece := Some (get_square r c)
    | Some x ->
      let i = get_image x in
      x##style##backgroundImage <- (Js.string i);
      x##style##backgroundImage <- (Js.string "none")
  end;
  Js._false

(* [square_callbacks l] registers callbacks for clicks on active board squares
 * listed in [l] *)
let rec square_callbacks l =
  if l <> [] then
    match l with
    | [] -> square_callbacks l
    | (r,c)::t -> (get_square r c)##onclick <- handler (handle_square r c);
      square_callbacks t

(* [handle_board _] is the callback for the customize board button *)
let handle_makeboard _ =
  current_stage := Custom_board;
  window##alert (Js.string "You are now customizing your board. You can click
  any squares on the board to turn them into a void. ");
  Js._false

(* [handle_piece _] is the callback for the customize piece button *)
let handle_makepiece _ =
  current_stage := Custom_piece;
  window##alert (Js.string "You are now customizing a new piece. You can click
  any squares on the board to represent possible squares the new piece can move
  to relative to its current position. ");
  Js._false

(* [handle_play _] is the callback for the start game button *)
let handle_play _ =
  current_stage := Play;
  now_playing ();
  window##alert (Js.string "You must now place your pieces on the board.
  Once you have done this, you can now play!");
  Js._false

let onload _ =
  now_playing ();
  (get_element "custom_board")##onclick <- (handler handle_makeboard);
  (get_element "custom_piece")##onclick <- (handler handle_makepiece);
  (get_element "play_game")##onclick <- (handler handle_play);
  Js._false

(* [create_state_json squares pieces custom] creates a JSON file for the
 * initial state of the game using the list of active squares [squares],
 * the initial position of pieces [pieces], and the movement pattern of the
 * customized piece [custom] *)
let create_state_json squares pieces custom =
  failwith "Unimpl"

let _ =
  window##onload <- (handler onload)
