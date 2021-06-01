open List
open Printf

let xs = Scanf.scanf "%d"(fun x -> x) 

let rec simplificar lista =
  match lista with
  | "V"::rl                      
  | "RP"::"E"::"OB"::"E"::"LP"::rl 
  | "RP"::"E"::"LP"::rl
  | "E"::"F"::rl			 -> simplificar ("E"::rl)	
  | "E"::"OB"::"E"::rl             -> simplificar ("E"::rl)
  | _                              -> lista

let rec ler t lista =
  if t=0 then lista
  else
    let c = Scanf.scanf " %s"(fun x -> x) in
    ler (t-1) ( simplificar (match c with
        | "("        -> "LP"::lista
        | ")"        -> "RP"::lista
        | "&"        -> "OB"::lista
        | "|"        -> "OB"::lista
        | "->"       -> "OB"::lista
        | "<->"      -> "OB"::lista
        | "!"        -> "F"::lista
        |_           -> "V"::lista
      ))


let  verificar lista =
  if lista = ["E"] then "YES"
  else  
    "NO"    

let  () = print_endline (verificar (ler xs []))