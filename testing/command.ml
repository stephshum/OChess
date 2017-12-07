open Models

type command =
  | Move of (position * position)
  | Promotion of name
  | Quit
  | Invalid
  | PreMove of position

let parse s =
  let requit = Str.regexp ".*quit" in
  let reexit = Str.regexp ".*exit" in
  let repre = Str.regexp "pre ([0-9]+,[0-9]+)" in
  let reprom = Str.regexp "prom" in
  let reMove = Str.regexp "move ([0-9]+,[0-9]+) ([0-9]+,[0-9]+)" in
  let reint = Str.regexp "[0-9]+" in
  let int_tuple_of_string st =
    try
      let pos = Str.search_forward reint st 0 in
      let fs = Str.matched_string st in
      let nxt = (String.sub st (String.length fs+pos)
                   (String.length s - String.length fs-pos)) in
      let _ = Str.search_forward reint nxt 0 in
      let sn = Str.matched_string nxt in
      (fs|>int_of_string,sn|>int_of_string)
    with
    | _ -> (-1,-1)
  in
  let two_int_tuple_of_string st =
    try
      let pos = Str.search_forward reint st 0 in
      let fs = Str.matched_string st in
      let nxt = (String.sub st (String.length fs+pos)
                   (String.length st - String.length fs-pos)) in
      let pos2 = Str.search_forward reint nxt 0 in
      let sn = Str.matched_string nxt in
      let nxt2 = (String.sub nxt (String.length sn+pos2)
                    (String.length nxt - String.length sn-pos2)) in
      let pos3 = Str.search_forward reint nxt2 0 in
      let trd = Str.matched_string nxt2 in
      let nxt3 = (String.sub nxt2 (String.length trd+pos3)
                    (String.length nxt2 - String.length trd-pos3)) in
      let _ = Str.search_forward reint nxt3 0 in
      let frth = Str.matched_string nxt3 in
      ((fs|>int_of_string,sn|>int_of_string),
       (trd|>int_of_string,frth|>int_of_string))
    with
    | _ -> ((-1,-1),(-1,-1))
  in
  let name_of_string st =
    let pos = Str.search_forward reprom st 0 in
    let fs = (Str.matched_string st)^" " in
    let namestr = (String.sub st (String.length fs+pos)
                     (String.length s - String.length fs-pos)) in
    if namestr = "rook" then Rook false
    else if namestr = "knight" then Knight
    else if namestr = "bishop" then Bishop
    else if namestr = "queen" then Queen
    else Custom namestr
  in
  if Str.string_match requit s 0 || Str.string_match reexit s 0 then Quit
  else if Str.string_match repre s 0 then
    PreMove (int_tuple_of_string s)
  else if Str.string_match reMove s 0 then
    Move (two_int_tuple_of_string s)
  else if Str.string_match reprom s 0 then
    Promotion (name_of_string s)
  else Invalid
