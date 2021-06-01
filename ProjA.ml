let (n, k, a, b) = Scanf.scanf "%d %d %d %d" (fun n k a b -> (n, k, a, b));; 

let movimentos = [(-2,-1)  ; (-2,+1) ;
                  (-1,-2)  ; (-1,+2) ;
                  (+1,-2)  ; (+1,+2) ;
                  (+2,-1)  ; (+2,+1)] ;; 

let tabuleiro (a,b) = (0<=a) && (a<n) && (0<=b) && (b<n) ;; 
if (tabuleiro (a,b) = false) then failwith "Valores invalidos" ;;
if (n < 1 || n > 50 ) then failwith "Valores invalidos";;
if (k < 1 || k > 8) then failwith "Valores invalidos";;

let counter = ref 0;;
let add (e,f) (x,z) = (e+x,f+z);;

let rec move k (a,b) = 
  if k=0 then counter := !counter + 1 else (
		for i = 0 to 7 do
			let pos = add (a,b) (List.nth movimentos i) in
				if tabuleiro pos  then 	
					move (k-1) pos
				else ()
		done;)

let () = move k (a,b)
let () = Printf.printf ("%d\n") (!counter);