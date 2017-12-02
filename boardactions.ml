open Yojson
open State
open Js_of_ocaml
open Dom

let document = Dom_html.window##.document

(* [stage] is the type representing what stage the users are in while using the
 * application *)
type stage = Custom_board | Custom_piece | Play

(* [current_stage] is the stage the users are in currently and starts initially
 * at Custom_board *)
let current_stage = ref Custom_board

(* [void_color] is the color of the background and void square *)
let void_color = "#24425b"
(* [dark_color] is the color of a dark square on the board after a power-up
 * has been used *)
let dark_color = "#edac93"

(* [init_piece_pos] is a list of the pieces placed on the board before a
 * game has begun. *)
let init_piece_pos : (string * (int * int)) list ref = ref []

(* [chosen_piece] is the piece picked to be moved on to the initial board *)
let chosen_piece = Dom_html.element Js.t option ref = ref None

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
  (get_button "custom_board")##disabled <- (Js.bool true);
  (get_button "custom_piece")##disabled <- (Js.bool true)

(* [enable_buttons ()] enables all buttons at the beginning of game setup *)
let enable_buttons () =
  (get_button "custom_board")##disabled <- (Js.bool false);
  (get_button "custom_piece")##disabled <- (Js.bool false);
  (get_button "play_game")##disabled <- (Js.bool false)
