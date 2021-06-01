open List

let (n, k, a, b) = Scanf.scanf "%d %d %d %d" (fun n k a b -> (n, k, a, b));; 

let movimentos = [(-2,-1)  ; (-2,+1) ;
                  (-1,-2)  ; (-1,+2) ;
                  (+1,-2)  ; (+1,+2) ;
                  (+2,-1)  ; (+2,+1)] ;; 

let tabuleiro (a,b) = (0<=a) && (a<n) && (0<=b) && (b<n) ;; 

if (tabuleiro (a,b)= false) then failwith "Valores invalidos" ;;
if (n < 1 || n > 50 ) then failwith "Valores invalidos";;
if (k > 8 || k < 1) then failwith "Valores invalidos";;

let add (e,f) (x,z) = (e+x,f+z);;

let move (a,b) =  
    let moves = movimentos in 
    fold_left add (a,b) movimentos;
    filter movimentos


let rec knight k positions= 
  if k=0 then () else (
  let pos = positions  in
  let positions = fold_left move positions;
  knight (k-1 , pos))

let () = move 0
let result = length pos ;;
let () = Printf.printf ("\n%d\n") (result);