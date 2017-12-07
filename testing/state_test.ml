open OUnit2
open State
open Models
open Command



let j = Yojson.Basic.from_file "default.json"
let a = init_state j
let b = do' (Move ((3,8),(3,6))) a
let c = do' (Move ((8,2), (7,4))) b
let d = do' (Move ((6,9), (6,8))) c


let tests = [
  "b_missing" >:: (fun _ -> assert_equal (a.missing) (b.missing));
  "b_pieces" >:: (fun _ -> assert_equal (a.pieces) (b.pieces));
  "b_pc_loc" >:: (fun _ -> assert_equal (List.sort compare
                                           (((3,6),{name = Pawn false;
                                                    pcolor = White})::
                                            (List.remove_assoc (3,8) a.pc_loc)))
                     (List.sort compare b.pc_loc));
  "b_captured" >:: (fun _ -> assert_equal (a.captured) (b.captured));
  "b_color" >:: (fun _ -> assert_equal (a.color) (if b.color = Black then White
                                                  else Black));
  "b_promote" >:: (fun _ -> assert_equal (a.promote) (b.promote));
  "b_trow" >:: (fun _ -> assert_equal (a.trow) (b.trow));
  "b_brow" >:: (fun _ -> assert_equal (a.brow) (b.brow));
  "b_turn" >:: (fun _ -> assert_equal (a.turn) (b.turn-1));
  "b_score" >:: (fun _ -> assert_equal (a.score) (b.score));
  "b_wking" >:: (fun _ -> assert_equal (a.wking) (b.wking));
  "b_bking" >:: (fun _ -> assert_equal (a.bking) (b.bking));
  "b_powvalid" >:: (fun _ -> assert_equal (a.powvalid) (b.powvalid));
  "b_check" >:: (fun _ -> assert_equal (a.check) (b.check));
  "b_checkmate" >:: (fun _ -> assert_equal (a.checkmate) (b.checkmate));

  "c_missing" >:: (fun _ -> assert_equal (c.missing) (b.missing));
  "c_pieces" >:: (fun _ -> assert_equal (c.pieces) (b.pieces));
  "c_pc_loc" >:: (fun _ -> assert_equal (List.sort compare c.pc_loc)
                     (List.sort compare (((3,6),{name = Knight;
                                                 pcolor = Black})::
                                         (List.remove_assoc (8,2) b.pc_loc))));
  "c_captured" >:: (fun _ -> assert_equal (c.captured) (b.captured));
  "c_color" >:: (fun _ -> assert_equal (c.color) (if b.color = Black then White
                                                  else Black));
  "c_promote" >:: (fun _ -> assert_equal (c.promote) (b.promote));
  "c_trow" >:: (fun _ -> assert_equal (c.trow) (b.trow));
  "c_brow" >:: (fun _ -> assert_equal (c.brow) (b.brow));
  "c_turn" >:: (fun _ -> assert_equal (c.turn) (b.turn+1));
  "c_score" >:: (fun _ -> assert_equal (c.score) (b.score));
  "c_wking" >:: (fun _ -> assert_equal (c.wking) (b.wking));
  "c_bking" >:: (fun _ -> assert_equal (c.bking) (b.bking));
  "c_powvalid" >:: (fun _ -> assert_equal (c.powvalid) (b.powvalid));
  "c_check" >:: (fun _ -> assert_equal (c.check) (b.check));
  "c_checkmate" >:: (fun _ -> assert_equal (c.checkmate) (b.checkmate));

  "d_missing" >:: (fun _ -> assert_equal (d.missing) (c.missing));
  "d_pieces" >:: (fun _ -> assert_equal (d.pieces) (c.pieces));
  "d_pc_loc" >:: (fun _ -> assert_equal (List.sort compare d.pc_loc)
                     (List.sort compare (((6,8),{name = Pawn false;
                                                 pcolor = White})::
                                         (List.remove_assoc (6,9) b.pc_loc))));
  "d_captured" >:: (fun _ -> assert_equal (d.captured) (c.captured));
  "d_color" >:: (fun _ -> assert_equal (d.color) (if c.color = Black then White
                                                  else Black));
  "d_promote" >:: (fun _ -> assert_equal (d.promote) (c.promote));
  "d_trow" >:: (fun _ -> assert_equal (d.trow) (c.trow));
  "d_brow" >:: (fun _ -> assert_equal (d.brow) (c.brow));
  "d_turn" >:: (fun _ -> assert_equal (d.turn) (c.turn+1));
  "d_score" >:: (fun _ -> assert_equal (d.score) (c.score));
  "d_wking" >:: (fun _ -> assert_equal (d.wking) (c.wking));
  "d_bking" >:: (fun _ -> assert_equal (d.bking) (c.bking));
  "d_powvalid" >:: (fun _ -> assert_equal (d.powvalid) (c.powvalid));
  "d_check" >:: (fun _ -> assert_equal (d.check) (c.check));
  "d_checkmate" >:: (fun _ -> assert_equal (d.checkmate) (c.checkmate));


]

let suite = "State test suite" >:::
            tests

let _ = run_test_tt_main suite
