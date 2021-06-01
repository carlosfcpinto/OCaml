open List
(*let n = read_int();;
let k = read_int();;
let a = read_int();;
let b = read_int();;*)

let (n, k, a, b) = Scanf.scanf "%d %d %d %d" (fun n k a b -> (n, k, a, b));; 

(*int_of_string (Sys.argv) (1-4)*)
let movimentos = [(-2,-1)  ; (-2,+1) ;
                  (-1,-2)  ; (-1,+2) ;
                  (+1,-2)  ; (+1,+2) ;
                  (+2,-1)  ; (+2,+1)] ;; 

let tabuleiro (a,b) = (0<=a) && (a<n) && (0<=b) && (b<n) ;; 

(* let counter = ref 1;; *)
let len = ref 0;;
let add (e,f) (x,z) = (e+x,f+z);;
let positions = ref [(a, b)] (*(filter (tabuleiro) (map (add (a,b)) movimentos))*);;
let added = ref (length !positions);;

let printlist l = iter (fun (x, y) -> Printf.printf "(%d, %d) " x y) l; print_newline ();;

let rec move counter= 
        if counter=k then () else (
        len := length !positions ;
        Printf.printf "Iteração %d:\n In: " counter;
        printlist !positions;
        let pos = !positions in
        positions := [];
        for i = (*!len- !added*) 0 to !len-1 do
        positions := !positions @ (filter (tabuleiro) (map (add (nth pos i)) movimentos));
        (*positions := sort_uniq compare !positions;*)
        done ;
        Printf.printf "Out: ";
        printlist !positions;
        added := length !positions - !len ;
        (*Printf.printf("%d") !added;*)
        (* counter := !counter + 1 ;*)
        move (counter + 1))
        (*if !counter<=k then (move ()) else () ;;*)
 

let () = move 0
(*let pos = sort_uniq compare !positions;;*)
let () = Printf.printf "RESULT: "; printlist !positions;;
let result = length !positions ;;
(*let pp_int_pair ppf (x,y) =
  Printf.fprintf ppf "(%d,%d)" x y
let () = iter (Printf.printf "%a" pp_int_pair) positions
let () = Printf.printf ("\n\n")
let () = iter (Printf.printf "%a" pp_int_pair) movimentos*)
let () = Printf.printf ("\n%d\n") (result);
         