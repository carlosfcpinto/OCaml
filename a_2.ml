let aux = read_line() 
          |> String.split_on_char ' ' 
          |> List.map int_of_string

let n, x = List.hd aux, List.hd(List.tl aux)
let hash = Hashtbl.create 99
let rec read n =
  if n = 0 then () else 
  let z = read_line()(*read_line() |> int_of_string*) in
  let (x,y) = (try Hashtbl.find hash z with Not_found -> (0,0)) in
  Hashtbl.replace hash z (x+1, n);  
  read (n-1)

let rec sum n values =
  if n = 0 then () else
  match values with
    |x::xs -> let (a,(b,c)) = x in
              print_endline a; sum (n-1) xs
    |[] -> ()


let () = read (3*n)

(*https://rosettacode.org/wiki/Word_frequency#OCaml*)
let l = Hashtbl.fold (fun word count acc -> (word, count)::acc) hash [] 
let s = List.sort (fun (_, (c1,c2)) (_, (c3, c4)) -> let z = compare c3 c1 in match z with |0 -> compare c2 c4 |_ -> z) l 
let () = sum x s