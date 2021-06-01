open List

let (n, k, a, b) = Scanf.scanf "%d %d %d %d" (fun n k a b -> (n, k, a, b));; 

let movimentos = [(-2,-1)  ; (-2,+1) ;
                  (-1,-2)  ; (-1,+2) ;
                  (+1,-2)  ; (+1,+2) ;
                  (+2,-1)  ; (+2,+1)] ;; 

let tabuleiro (a,b) = (0<=a) && (a<n) && (0<=b) && (b<n) ;; 

let len = ref 0;;
let add (e,f) (x,z) = (e+x,f+z);;
let positions = ref [(a, b)];;

let rec move counter= 
  if counter=k then () else (
    len := length !positions ;
    let pos = !positions in
    positions := [];
    for i = 0 to !len-1 do
      positions := !positions @ (filter (tabuleiro) (map (add (nth pos i)) movimentos));
    done;
    move (counter + 1))

let () = move 0
let result = length !positions ;;
let () = Printf.printf ("\n%d\n") (result);