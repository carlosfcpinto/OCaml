let (w, z) = Scanf.scanf "%d %d\n" (fun x z -> (x,z))

let rec read n part =
  if n = 0 then part else 
  let z = Scanf.scanf "%d\n" (fun x -> x)(*read_line() |> int_of_string*) in read (n-1) (z::part)

let rec auxfunction x n part =
  match part with
  |hd::tl -> if n = x then (hd + (z*(w-1)))::(auxfunction x (n-1) tl) else (hd-z)::(auxfunction x (n-1) tl)
  |[] -> []

let rec get_last = function
|x::[] -> x
|x::xs -> get_last xs
|[] -> 0
let rec trade n count part =
  if n = 0 then (get_last (List.sort compare count)) else
  let part2 = List.sort compare (auxfunction (w-n + 1) w part) in
  let () = List.iter (fun x -> Printf.printf "%d " x) (auxfunction (w-n +1) w part) in
  let aux = List.hd (part2) in
  trade (n-1) (aux::count) (List.rev (auxfunction (w-n + 1) w part))

let fin = read w []
let fini = trade (w) [] fin
let () = Printf.printf "%d\n" fini