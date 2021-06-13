exception Not_Can of int
let number_coin = read_int()

(*Fonte:"Introdução à Programação Funcional em OCaml"
Mário Pereira and Simão Melo de Sousa, página 109
Função de memoização automática para uma função recursiva genérica
Função utilizada para optimizar o programa e evitar computações de uma valor já calculado
A função memo aceita a função alvo da memoização e devolve esta mesma função provida agora do mecanismo de memoização*)
let memo func =
  let table = Hashtbl.create 999 in
  let rec f v =
    try Hashtbl.find table v
    with Not_found ->
      let ret = func f v in
      Hashtbl.add table v ret;
      ret
  in
  f

let rec currency = fun x ->
  match x with
  0 -> []
  |_ -> read_int()::currency(x - 1)

let range = fun x -> if List.length x < 3 then (0,0) else (((List.nth x (List.length x - 3)) + 1), (List.hd x + List.nth x 1))

(*https://sodocumentation.net/algorithm/topic/3140/greedy-algorithms currency is in decreasing order
  Adicionada função de memoização para evitar computações desnecessárias do mesmo valor, caso encontremos o mesmo*)
let change_make_greedy = fun greedy currency amount ->
  if amount = 0 then 0
    else 
      (* we find the first value smaller or equal to the remaining amount *)
      let coin = List.find ((>=) amount) currency in
      (*print_endline("\n\nI JUST GAVE YOU " ^ string_of_int(coin));*)
      1 + greedy currency (amount - coin)

(*https://www.geeksforgeeks.org/find-minimum-number-of-coins-that-make-a-change/*)
let change_make_dynamic ammount currency = 
  let coinReq = Array.make (ammount + 1) (number_coin*100) in
  coinReq.(0) <- 0;
  for amt = 1 to (ammount) do
    List.iter (fun x -> if x <= amt then let aux = coinReq.(amt - x) in 
                if (aux <> (number_coin*100) && aux + 1 < coinReq.(amt)) then
                coinReq.(amt) <- aux +1) currency
    (*for j = 0 to (List.length currency - 1) do
      if ((List.nth currency j) <= amt) then
        let sub_res = coinReq.(amt - (List.nth currency j)) in
        if (sub_res <> (number_coin*100) && sub_res + 1 < coinReq.(amt)) then
                    (coinReq.(amt) <- sub_res + 1)
      (*coinReq.(amt) <- (min (coinReq.(amt - (List.nth currency j)) + 1) coinReq.(amt))*)
    done;*)
  done;
  coinReq


let () = 
  let curr = currency number_coin in
  let (a,b) = range curr in
  let greedy_memo = memo change_make_greedy in
  let dyn = change_make_dynamic b curr in
  (*print_endline(string_of_int a); print_endline(string_of_int b);*)
  try
    for i = a to b do
      (*print_endline(string_of_int(change_make_greedy curr i) ^ "\t" ^ string_of_int(dyn.(i)));*)
      if ((greedy_memo curr i) <> (dyn.(i))) then raise (Not_Can i)
    done;
    print_endline("YES")
  with Not_Can x ->  print_endline (string_of_int x)