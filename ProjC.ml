let size = Scanf.sscanf (read_line()) "%d" (fun size -> (size))

let rec check1 lists n=
  if(n == 0) then (1) else 
  (let s = List.nth lists (n-1) in
  let x = (String.make 1 '~' ^ s) in
  if((List.mem (x) lists) || (List.mem ("1") lists)) then (0) 
  else (check1 lists (n-1)))

let rec check2 list_of_list=
  match list_of_list with
  |[] -> 0
  |el::li -> if (check1 el ((List.length el)) == 0) then (check2 li) else (1)

let rec inp n list=(
  if n == 0 then (list) else (
    let s = (String.split_on_char ' ' (read_line())) in
    inp (n-1) ([s]@list)))

let lista = inp size []
let () = if(check2 lista == 0) then (Printf.printf"VALIDA\n") 
else (Printf.printf"NAO E VALIDA\n")