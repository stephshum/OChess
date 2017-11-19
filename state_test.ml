open OUnit2
open State

let tests = []

let suite = "State test suite" >:::
  tests

let _ = run_test_tt_main suite
