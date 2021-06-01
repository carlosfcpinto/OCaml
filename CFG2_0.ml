type symbol = Epsilon | Terminal of string | NonTerminal of string | HALT

let trans = function
  | "_" -> Epsilon
  | c -> if c = String.uppercase_ascii c then NonTerminal c else Terminal c

let untrans = function
  | Epsilon -> ""
  | Terminal x -> x
  | NonTerminal z -> z
  | _ -> ""

let max = read_int ()

let m = read_int ()

let hash = Hashtbl.create m

;;
for i = 1 to m do
  let aux = List.map trans (String.split_on_char ' ' (read_line ())) in
  Hashtbl.add hash (List.hd aux) (List.tl (List.tl aux))
done

let rec count_terminals lista =
  match lista with
  | [] -> 0
  | x :: xs -> (
      match x with
      | Terminal x -> count_terminals xs + 1
      | _ -> count_terminals xs + 0)

let merge_lists l p =
  let t = ref l in
  List.iter (fun e -> t := !t @ [ e ]) p;
  !t

let rec productor hash symb count_ter : symbol list list =
  if count_ter >= max + 10 then [ [ HALT ] ]
  else
    match symb with
    | hd :: tl -> (
        match hd with
        | Terminal x ->
            List.map (fun z -> hd :: z) (productor hash tl (count_ter + 1))
        | NonTerminal z ->
            let w = expander hash hd count_ter in
            let f = productor hash tl count_ter in
            List.concat
              (List.map (fun x -> List.map (fun m -> List.append x m) f) w)
        | Epsilon -> productor hash tl count_ter
        | HALT ->
            List.map (fun y -> hd :: y) (productor hash tl (count_ter + 1)))
    | [] -> [ [] ]

and expander (hash : (symbol, symbol list) Hashtbl.t) x count_ter =
  if count_ter >= max + 10 then [ [ HALT ] ]
  else
    let z = try Hashtbl.find_all hash x with Not_found -> [] in
    List.map
      (fun x -> List.concat (productor hash x (count_terminals x + count_ter)))
      z

(*match z with
  |hd::tl -> (productor hash hd 0)::(expander hash tl count_ter)
  |[] -> []*)

let string_of_list list =
  let buf = Buffer.create 16 in
  List.iter (fun x -> Buffer.add_string buf (untrans x)) list;
  Buffer.contents buf

let printlist l = List.iter (fun x -> Printf.printf "%s\n" x) l

let x = expander hash (trans "S") 0

let x2 = List.filter (fun x -> not (List.mem HALT x)) x

let x3 =
  List.sort String.compare
    (List.map string_of_list (List.filter (fun x -> List.length x <= max) x2))

let () = printlist x3

(*let () = List.iter (fun y-> printlist y) (List.map (fun z -> List.map untrans z)(expander hash (trans "S") 0))*)
(*let () = List.iter (fun x -> Printf.printf"%s\n" x) (List.map (fun z -> string_of_list z) (List.map (fun x -> List.map untrans x) 
        (List.filter(fun x -> List.length x <= max)(expander hash (trans "S") 0))))*)
(*let () = Printf.printf "%s\n" (string_of_list((List.map untrans (List.concat ( List.filter (fun x -> List.length x <= max)(expander hash (trans "S") 0))))))*)
