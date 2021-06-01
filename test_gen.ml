let () =
  let () = Random.self_init () in
  let t_1 = (int_of_string Sys.argv.(1)) in
  let t_2 = (int_of_string Sys.argv.(2)) in
  let s_1 = String.init t_1 (fun _ -> match Random.int 4 with 0 -> 'A' | 1 -> 'C' | 2 -> 'G' | 3 -> 'T' | _ -> assert false) in
  let s_2 = String.init t_2 (fun _ -> match Random.int 4 with 0 -> 'A' | 1 -> 'C' | 2 -> 'G' | 3 -> 'T' | _ -> assert false) in
  let () = print_endline s_1 in
  print_endline s_2