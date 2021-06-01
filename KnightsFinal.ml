open List
open Array.Core_array

let (n, k, a, b) = Scanf.scanf "%d %d %d %d" (fun n k a b -> (n, k, a, b));; 

let movimentos = [|(-2,-1)  ; (-2,+1) ;
                  (-1,-2)  ; (-1,+2) ;
                  (+1,-2)  ; (+1,+2) ;
                  (+2,-1)  ; (+2,+1)|] ;; 

let tabuleiro (a,b) = (0<=a) && (a<n) && (0<=b) && (b<n) ;; 
if (tabuleiro (a,b)= false) then failwith "Valores invalidos" ;;
if (n < 1 || n > 50 ) then failwith "Valores invalidos";;
if (k < 1 || k > 8) then failwith "Valores invalidos";;

let time f =
  let t = Sys.time () in
  let res = f () in
  Printf.printf "Execution time: %f seconds\n"
    (Sys.time () -. t);
  res;;

let add (e,f) (x,z) = (e+x,f+z);;
let positions = ref [|(a, b)|];;
let z = ref 0;;

let rec move k= 
  if k=0 then () else (
    let len = Array.length !positions in 
    let pos = !positions in
    positions := [||];
    for i = 0 to len-1 do
      for x = 0 to 7 do
        z := !z + 1;
        !positions.(!z) <- (add pos.(i) movimentos.(x));
      done;
    done; 
    Printf.printf("Iteração\n");
    move (k - 1)) 

let () = time(fun()->move k);; 
let result = Array.length !positions ;;
let () = Printf.printf ("%d\n") (result); 