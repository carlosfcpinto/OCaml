let tamanho = read_int()

let rec ler = function
  0 -> []
  |_ as x -> read_int() :: ler (x-1)

let rec greedy currency_list change n = 
  if change = 0 then n
        else 
          (* we find the first value smaller or equal to the remaining amount *)
          let coin = List.find ((>=) change) currency_list in
          greedy currency_list (change - coin) (n+1)

    
          let optimal change currency = 
            let coinReq = Array.make (change + 1) (100000) in
            coinReq.(0) <- 0;
            for amt = 1 to (change) do
            for j = 0 to (List.length currency - 1) do
                if ((List.nth currency j) <= amt) then
                  let sub_res = coinReq.(amt - (List.nth currency j)) in
                  if (sub_res <> (100000) && sub_res + 1 < coinReq.(amt)) then
                              (coinReq.(amt) <- sub_res + 1)
                  done;
                done;
              coinReq.(change)

(*let rec optimal currency_list change sack n =
  match change with
    |  [] -> optimal currency_list sack [] (n + 1)
    |  x  :: xi -> let aux_list = List.append sack (List.map (fun z -> x-z) currency_list ) in 
                   if List.exists ((=) 0) aux_list then n else optimal currency_list xi aux_list n*)

let rec aux_ver lower_bound upper_bound lista=
    if lower_bound = upper_bound then -1 else(
    if ((greedy lista lower_bound 0) <> (optimal lower_bound lista)) then lower_bound else 
      (aux_ver (lower_bound + 1) upper_bound lista))
  
  let verify lista = 
  if List.length lista >= 3 then (aux_ver ((List.nth lista (List.length lista - 3)) + 1) (List.hd lista + List.nth lista 1) lista) else (aux_ver 0 0 lista)
    

let () = ler tamanho |> verify |> function -1 -> print_endline("YES") |_ as x -> print_endline((string_of_int(x)))