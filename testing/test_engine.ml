open State
open Command
open Models


(* [print_pc name] prints the piece name to terminal *)
let print_pc = function
  | Rook _ -> print_endline "Rook"
  | Knight -> print_endline "Knight"
  | Bishop -> print_endline "Bishop"
  | Queen -> print_endline "Queen"
  | King _ -> print_endline "King"
  | Pawn _ -> print_endline "Pawn"
  | Custom s -> print_endline s

(* [str_of_color color] returns the [color] as a string *)
let str_of_color = function
  | Black -> "Black"
  | White -> "White"

(* [str_of_pc name] returns the piece name as a string *)
let str_of_pc = function
  | Rook _ -> "Rook"
  | Knight -> "Knight"
  | Bishop -> "Bishop"
  | Queen -> "Queen"
  | King _ -> "King"
  | Pawn _ -> "Pawn"
  | Custom s -> s

(* [str_of_pc_loc acc pc_loc] returns a string of pieces and their positions *)
let rec str_of_pc_loc acc pc_loc =
  match pc_loc with
  | [] -> acc
  | ((x,y),pc)::t ->
    let pos = "("^(string_of_int x)^","^(string_of_int y)^")\n" in
    str_of_pc_loc (acc^(str_of_color pc.pcolor)^" "^
                   (str_of_pc pc.name)^" "^pos) t

(* [str_of_loc loc] returns  the location as a string *)
let rec str_of_loc (x,y) =
  "("^(string_of_int x)^","^(string_of_int y)^")\n"

(* [str_of_cap acc cap] returns names of the color and pieces captured *)
let rec str_of_cap acc cap =
  match cap with
  | [] -> acc
  | pc::t ->
    str_of_cap (acc^(str_of_color pc.pcolor)^" "^
        (str_of_pc pc.name)^"\n") t

(* [str_of_score score] returns a formatted string of score *)
let str_of_score (w,b) =
  "White :"^(string_of_int w)^", Black: "^(string_of_int b)^"\n"

(* [str_of_moves acc moves] returns a string of valid move locations *)
let rec str_of_moves acc moves =
  match moves with
  | [] -> acc
  | (x,y)::t ->
    str_of_moves (acc^" ("^(string_of_int x)^","^(string_of_int y)^")") t

(* [check_str check] returns the string of the color or none if [None] *)
let check_str = function
  | None -> "None"
  | Some Black -> "Black"
  | Some White -> "White"

(* [game_loop game] is the repl used to test chess in terminal *)
let rec game_loop game =
  (*print_endline "Pieces in play:\n";
    print_endline ((game.pc_loc |> str_of_pc_loc "["))^"]\n";*)
  print_endline "Pieces captured\n";
  print_endline ((game.captured |> str_of_cap "[")^"]\n");
  print_endline "Score: ";
  print_endline (game.score |> str_of_score);
  print_endline "Check: ";
  print_endline (game.check |> check_str);
  print_endline "WKing Loc: ";
  print_endline (game.wking |> str_of_loc);
  print_endline "BKing Loc: ";
  print_endline (game.bking |> str_of_loc);
  print_endline ("Turn: "^(game.turn |> string_of_int));
  print_endline (game.color |> str_of_color);
  if game.checkmate <> None then
    match game.checkmate with
    | Some Black -> print_endline "White Won!"
    | Some White -> print_endline "Black Won!"
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
              if  game.pc_loc |>
                  List.filter (fun (_,pce) -> pce.pcolor = game.color) |>
                  List.mem_assoc pi then
                begin
                  let mv_lst = val_move_lst pi game in
                  print_pc (List.assoc pi game.pc_loc).name;
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

(* [play_game f] attempts to create a state from json and if succesful,
 *  initiates game_loop. Prints fail message if unsuccessful. *)
let play_game f =
  try
    let game = f |> Yojson.Basic.from_file |> init_state in
    print_endline (f^" is a valid game \n");
    game_loop game
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
