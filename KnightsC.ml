open Printf
open List

(* ler inputs *)
let n = Scanf.scanf " %d" (fun x -> x)                              (* lê o tamanho do tabuleiro *)
let jumps = Scanf.scanf " %d" (fun x -> x)                          (* lê o número máximo de saltos *)
let read_tuple () = Scanf.scanf " %d %d" (fun x y -> x,y)           (* lê um tuplo -> neste caso serve para ler a posição inicial do cavalo *)

(* guardar a posição inicial *)
let start = read_tuple ()

(* lista com os movimentos possíveis de um cavalo num jogo de xadrez *)
let moves = [(-2, -1); (-2, 1); (-1, -2); (-1, 2); (1, -2); (1, 2); (2, -1); (2, 1)];;

(* efetua o salto do cavalo dado a sua posição e o tuplo que descreve o seu movimento *)
let jump (px, py) (movx, movy) = (px+movx, py+movy);;

(* verifica se um salto é válido, ou seja se a posição que resultou está dentro (dos limites) do tabuleiro -> devolve verdade se for válida *)
let valid (x, y) = if x >= 0 && x < n && y >=0 && y < n then true else false;;

(* calcula a lista dos movimentos possíveis válidos a partir de uma dada posição *)
let possible p = List.fold_left (fun l mov -> if valid (jump p mov) then (jump p mov) :: l else l) [] moves ;; 

if (valid(start)= false) then failwith "Valores invalidos" ;;
if (n < 1 || n > 50 ) then failwith "Valores invalidos";;
if (jumps > 8 || jumps < 1) then failwith "Valores invalidos";;


(* andar pelo tabuleiro *)
let rec jumping jumps p = 
  if jumps = 0 then length(p)
  else 
    let newlist = ref [] in
    for i = 0 to (length(p)-1) do
      newlist := possible (nth p i) @ !newlist
    done ;
    jumping (jumps-1) !newlist

let () = print_int (jumping jumps [start])
let () = print_newline ()