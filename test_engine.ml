open State
open Command
open Models

(* TODO SPEC *)
let rec str_of_moves acc moves =
  match moves with
  | [] -> acc
  | (x,y)::t ->
    str_of_moves (acc^"; ("^(string_of_int x)^","^(string_of_int y)^")") t

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
        let txtcmd = String.lowercase_ascii text in
        if game.promote = None then
          let cmd = ("pre "^txtcmd) |> parse in
          match cmd with
          | PreMove pi ->
            begin
              if List.mem_assoc pi game.pc_loc then
                begin
                  (*print_endline (game |> val_move_lst pi |> str_of_moves "");*)
                  print_endline (val_move_lst pi game |> List.length |> string_of_int);
                  ANSITerminal.(print_string [green] "\n> ");
                  match read_line() with
                  | exception End_of_file -> ()
                  | text2 ->
                    begin
                      let cmd2 = ("move "^txtcmd^" "^
                                  (text2 |> String.lowercase_ascii)) |> parse in
                      match cmd2 with
                      | Move (pi,pf) ->
                        begin
                          if List.mem pf (val_move_lst pi game) then
                            game_loop (do' (Move (pi,pf)) game)
                          else
                            print_endline "Invalid move"; game_loop game
                        end
                      | Quit -> print_endline "You have quit the game"
                      | _ -> print_endline "Invalid command"; game_loop game
                    end
                end
              else
                print_endline "Please select a valid piece to move";
                game_loop game
            end
          | Quit -> print_endline "You have quit the game"
          | _ -> print_endline "Invalid command"; game_loop game
        else
          let cmd = ("prom "^txtcmd) |> parse in
          match cmd with
          | Promotion name ->
            begin
              if name <> King && name <> Pawn
                 && List.mem_assoc name game.pieces then
                game_loop (do' (Promotion name) game)
              else
                print_endline "Invalid promote"; game_loop game
            end
          | Quit -> print_endline "You have quit the game"
          | _ -> print_endline "Invalid command"; game_loop game
      end

(* TODO SPEC *)
let play_game f =
  try
    let game = f |> Yojson.Basic.from_file |> init_state in
    print_endline (f^" is a valid game");
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
