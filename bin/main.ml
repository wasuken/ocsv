let simple_csv_parse path delim =
  let ic = open_in path in
  let header_cols = ref [] in
  let values = ref [] in
  try
    header_cols := Str.split (Str.regexp delim) (input_line ic);
    while true do
      values := (Str.split (Str.regexp delim) (input_line ic)) :: !values
    done; ([], [])
  with
    End_of_file -> (!header_cols, List.rev !values);;

let csv_stats path delim =
  let (_, values) = simple_csv_parse path delim in
  let vmax i l = List.fold_left (fun acm x -> max acm (int_of_string (List.nth x i))) 0 l in
  let _ = Printf.printf "size: %d\n" (List.length values) in
  Printf.printf "max(%d): %d\n" 0 (vmax 0 values);;

let simple_parse_test path delim =
  let (hd_cols, values) = simple_csv_parse path delim in
  let ppl l = Printf.printf "%s\n" ((List.hd l) ^ (List.fold_left (fun acm x -> acm ^ "," ^ x) "" (List.tl l))) in
  let rec lp n f =
    let _ = f () in if n >= 0 then lp (n-1) f
  in
  let pf () = print_char '-' in
  let _ = ppl hd_cols in
  let _ = lp (List.length hd_cols) pf in
  let _ = print_endline "" in
  List.iter ppl values;;

let () =
  csv_stats "./test.csv" ","
