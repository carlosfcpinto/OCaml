open List
open Hashtbl

type symbol = |Epsilon
              |Terminal of string
              |NonTerminal of string
let trans = function "_" -> Epsilon |c -> if (c = (String.uppercase_ascii c)) then NonTerminal(c) else Terminal(c)
let untrans = function Epsilon -> "" |Terminal x -> x|NonTerminal z -> z

let rec grammarinput i lista hash= 
  match i with
  |0 -> lista
  |_ -> let s = map trans (String.split_on_char ' ' (read_line())) in 
    let () = (add hash (hd s) (tl(tl s))) in
    let l = rev_append lista (filter (fun x -> not (List.mem x lista))(map (function x -> match x with Terminal x -> x |_-> "") s)) in
    grammarinput (i-1) l hash

let rec count_terminals lista = 
  match lista with 
  |[]-> 0
  |x::xs-> match x with 
          |Terminal x -> (count_terminals xs) + 1
          |_ -> (count_terminals xs) + 0

(* Pesquisa recursiva dando como argumento todas as produções do símbolo onde estamos,
confirmando o numéro de terminais que temos com a length máxima da string permitida,
sendo strings a lista das strings possíveis com tamanho menor que i*)
let rec check_word lista =
  match lista with
  | [] -> true
  | x :: xs -> match x with
    |NonTerminal x -> false 
    |Terminal x -> check_word xs
    |Epsilon -> check_word xs


let rec worker2 x aux_left hash aux_xs=  
  match x with
  | [] -> aux_xs
  | y::ys -> match y with
    | Epsilon ->  worker2 ys (aux_left) hash aux_xs
    | Terminal z -> let aux_left = append aux_left [y] in worker2 ys aux_left hash aux_xs
    | NonTerminal c ->  let aux_xs2 = (let wtf z = (append (rev_append aux_left z) ys) in append aux_xs (map wtf ((find_all hash y)))) in
      worker2 ys aux_left hash aux_xs2

let rec worker starter hash max_size ender =
  match starter with
  | [] -> ender
  | x::xs -> if count_terminals x >= max_size then worker xs hash max_size ender  else
if check_word x then worker xs hash max_size (rev_append ender [x]) else 
let aux = (worker2 x [] hash starter) in worker aux hash max_size ender


let n = read_int()
let z = read_int()
let hash = Hashtbl.create 50
let l = grammarinput z [] hash
let ll = worker (find_all hash (trans "S"))hash n []


let rec tostring (lista:symbol list) = 
  match lista with 
  |[]->""
  |x::xs -> match x with 
    |Terminal z -> z
    |_ -> ""

let ll2 = List.map (fun x -> tostring x) ll

let ll2 = List.sort String.compare (List.sort_uniq String.compare ll2)

let () = List.iter (fun x -> Printf.printf"%s\n" x) ll2