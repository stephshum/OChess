open State
open Command
open Models

let str_of_color = function
  | Black -> "Black"
  | White -> "White"

let str_of_pc = function
  | Rook _ -> "Rook"
  | Knight -> "Knight"
  | Bishop -> "Bishop"
  | Queen -> "Queen"
  | King _ -> "King"
  | Pawn _ -> "Pawn"
  | Custom s -> s

let rec str_of_pc_loc acc pc_loc =
  match pc_loc with
  | [] -> acc
  | ((x,y),pc)::t ->
    let pos = "("^(string_of_int x)^","^(string_of_int y)^")\n" in
    str_of_pc_loc (acc^(str_of_color pc.pcolor)^" "^
                   (str_of_pc pc.name)^" "^pos) t

let rec str_of_cap acc cap =
  match cap with
  | [] -> acc
  | pc::t ->
    str_of_cap (acc^(str_of_color pc.pcolor)^" "^
        (str_of_pc pc.name)^"\n") t

let str_of_score (w,b) =
  "White :"^(string_of_int w)^", Black: "^(string_of_int b)

(* TODO SPEC *)
let rec str_of_moves acc moves =
  match moves with
  | [] -> acc
  | (x,y)::t ->
    str_of_moves (acc^" ("^(string_of_int x)^","^(string_of_int y)^")") t

(* TODO SPEC *)
let rec game_loop game =
  (*print_endline "Pieces in play:\n";
    print_endline (game.pc_loc |> str_of_pc_loc "");*)
  print_endline "Pieces captured\n";
  print_endline (game.captured |> str_of_cap "");
  print_endline "Score: ";
  print_endline (game.score |> str_of_score);
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
                  let mv_lst = val_move_lst pi game in
                  print_endline (mv_lst |> str_of_moves "");
                  ANSITerminal.(print_string [green] "\nto where? > ");
                  match read_line() with
                  | exception End_of_file -> ()
                  | text2 ->
                    begin
                      let cmd2 = ("move "^txtcmd^" "^
                                  (text2 |> String.lowercase_ascii)) |> parse in
                      match cmd2 with
                      | Move (pi,pf) ->
                        begin
                          if List.mem pf mv_lst then
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
              if List.mem_assoc name game.pieces then
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
