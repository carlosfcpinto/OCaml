(** Algoritmo de MacNaughton - Yamada para calcular uma expressÃ£o regular a partir de um autÃ³mato **)
open List
open Scanf
open Array


(* DefiniÃ§Ã£o dos tipos de dados para os autÃ³matos e para as expressÃµes regulares *)
(* TODO!!!! type .... type automato = .... *)
    
type regexp =
 | V  
 | E
 | C of char
 | U of regexp * regexp 
 | P of regexp * regexp 
 | S of regexp    

type automato = {states : int ; transitions : regexp array array array ; initial : int array ; final : int array}

(* simple pretty printing function *)
let rec string_of_regexp s =
  match s with
  | V       -> "0"
  | E       -> "1"
  | C  c    -> String.make 1 c    
  | U (f,g) -> "("^(string_of_regexp f)^" + "^(string_of_regexp g)^")"
  | P (f,g) -> "("^(string_of_regexp f)^" . "^(string_of_regexp g)^")"
  | S s     -> (string_of_regexp s)^"*"

(* definiÃ§Ãµes das funÃ§Ãµes de leitura *)
(* ... TODO!!! ... *)
(* leitura dos dados de input *)
let leitura () = (* TODO *) assert false


(* "max" = nÃºmero de estados do autÃ³mato "maq" *)
let max,maq = leitura ()


(*FunÃ§Ãµes de calculo da expressÃ£o regular a partir do autÃ³mato "maq"*)

let mat = Array.init (max+1) (fun _ -> Array.init (max+1) (fun _ -> (Array.init (max+2) (fun _ -> V))))

(* normalize l = l sem duplicados, de forma eficiente ie. linear! *) 
let normalize l =
  let tbl = Hashtbl.create (List.length l) in
  let f l e = try let _ = Hashtbl.find tbl e in l
              with Not_found ->  Hashtbl.add tbl e (); e::l
  in  List.rev (List.fold_left f [] l)

(*
simplify= funÃ§Ã£o que simplifica "um pouco" a expressÃ£o regular
realisa uma simplificaÃ§Ã£o maior do que a que foi sugerida no enunciado do problema
*)  
let rec simplify (a:regexp) = 
 match a with 
 | U (r,s) ->
   let sr = simplify r in
   let ss = simplify s in
   if sr = V then ss
   else if ss = V then sr
   else if ss = sr then sr
   else U (sr,ss) 
 | P (r,s) ->
   let sr = simplify r in
   let ss = simplify s in
   if sr = V then V
   else if ss = V then V
   else if sr = E then ss
     else if ss = E then sr
   else P (sr,ss) 
 | S r -> let sr = simplify r in
   if sr = V || sr = E 
   then E else (
     match sr with
       U (E,rr) | U (rr,E) -> S rr       
       | _ -> S sr
     )
 |  _ -> a
 
(* TODO!!!  - calcular a expressÃ£o regular - funÃ§Ãµes em falta aqui*)



(*calculo efecivo da expressÃ£o regular resultante, a partir das funÃ§Ãµes cuja definiÃ§Ã£o se espera  *)
let result = V  (*TODO!!!!! (substituir "V" pelo cÃ³digo em falta *)
  

(* vizualizaÃ§Ã£o do resultado, simplificado *)
let () = result |> simplify |> string_of_regexp |> print_endline
(* equivalente a: let () = print_endline (string_of_regexp (simplify result)) *)
    