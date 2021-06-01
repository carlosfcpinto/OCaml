let explode s =
  let rec expl i l = if i < 0 then l else expl (i - 1) (s.[i] :: l) in
  expl (String.length s - 1) []

let string_of_chars chars =
  let buf = Buffer.create 16 in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf

let reset, incr, get =
  let cont = ref 0 in
  let r () = cont := 0 in
  let i () = cont := !cont + 1 in
  let g () = !cont in
  (r, i, g)

let cardinals fitaaux =
  let rec cardinals2 fitadir =
    match fitadir with
    | [] -> []
    | x :: xs -> (
        match try List.hd xs with hd -> '0' with
        | '#' -> if x = '#' then cardinals2 xs else List.rev fitadir
        | _ -> List.rev fitadir)
  in
  cardinals2 ([ '#' ] @ List.rev fitaaux)

let rec turing2 fitaesqaux fitadiraux finalstatesaux hash i =
  let m = try List.hd fitadiraux with hd -> '#' in
  let z, y, w =
    try Hashtbl.find hash (i, m) with Not_found -> (' ', ' ', 0)
  in
  if List.mem i finalstatesaux then (true, List.rev_append fitaesqaux fitadiraux)
  else if get () > 200 || (z, y, w) = (' ', ' ', 0) then
    (false, List.rev_append fitaesqaux fitadiraux)
  else
    let () = incr () in
    match y with
    | 'R' ->
        let fitaesqaux = List.append [ z ] fitaesqaux in
        let n = try List.tl fitadiraux with tl -> [ '#' ] in
        turing2 fitaesqaux n finalstatesaux hash w
    | 'L' ->
        let b = try List.hd fitaesqaux with hd -> '0' in
        let n = try List.tl fitadiraux with tl -> [] in
        let fitadiraux = [ z ] @ n in
        if b = '0' then (false, fitadiraux)
        else
          let fitadiraux = List.append [ b ] fitadiraux in
          let v = try List.tl fitaesqaux with tl -> [] in
          turing2 v fitadiraux finalstatesaux hash w
    | _ -> (false, List.rev_append fitaesqaux fitadiraux)

let fita = explode (read_line ())

let n = int_of_string (read_line ())

let hash = Hashtbl.create n

let nf = int_of_string (read_line ())

let finalstates =
  List.map (fun x -> int_of_string x) (String.split_on_char ' ' (read_line ()))

let m = int_of_string (read_line ())

let () =
  for i = 1 to m do
    let (n, k), (a, b, z) =
      Scanf.sscanf (read_line ()) "%d %c %c %c %d" (fun n k a b z ->
          ((n, k), (a, b, z)))
    in
    Hashtbl.add hash (n, k) (a, b, z)
  done

let a, final = turing2 [] fita finalstates hash 1

let xz = get ()

let () =
  if xz > 200 then Printf.printf "DON'T KNOW\n"
  else
    let string_final = string_of_chars (cardinals final) in
    if a then Printf.printf "YES\n%s\n%d\n" string_final xz
    else (Printf.printf "NO\n%s\n%d\n") string_final xz
