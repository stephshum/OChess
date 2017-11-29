open State
open Command
open Models

(* TODO SPEC *)
let str_of_moves moves =
  failwith "Unimplemented"

(* TODO SPEC *)
let rec game_loop game =
  if game.checkmate <> None then
    match game.checkmate with
    | Some Black -> print_endline "Black Won!"
    | Some White -> print_endline "White Won!"
    | None -> failwith "Impossible"
  else
    ANSITerminal.(print_string [blue] "\n> ");
    match read_line () with
    | exception End_of_file -> ()
    | text ->
      begin
        if game.promote = None then
          let cmd = text |> parse in
          match cmd with
          | PreMove p ->
            begin
              ()
            end
          | Move (_,_) -> print_endline "Choose piece to move"; game_loop game
          | Promotion _ -> print_endline "Invalid command"; game_loop game
          | Quit -> print_endline "You have quit the game"
          | Invalid -> print_endline "Invalid command"; game_loop game
        else
          failwith "Promotion stuff here"
      end

(* TODO SPEC *)
let play_game f =
  try
    let game = f |> Yojson.Basic.from_file |> init_state in
    print_endline (f^"is a valid game");
    game_loop game;
    ()
  with
  | _ -> print_endline(f^" is not a valid game file"); ()

(* [main ()] starts the REPL, which prompts for a game to play.
 * You are welcome to improve the user interface, but it must
 * still prompt for a game to play rather than hardcode a game file. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the Test oChess Game engine.\n");
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game file_name

let () = main ()
