let movimentos = [ (1,2); (1,-2); (-1,2); (-1,-2); (2,1); (2,-1); (-2,1); (-2,-1)]

let count= ref 0

let soma (a,b) (c,d) = (a+c,b+d)

let (n,k,x, y) = Scanf.scanf "%d %d %d %d" (fun z w c v -> (z,w,c,v))

let rec recur (x,y) k =

if k = 0 then count:= !count +1

else (for i = 0 to 7 do

let (p,q) = soma (x,y) (List.nth movimentos i) in

let valido = (p>=0) && (p<n) && (q>=0) && (q<n) in

if valido then recur (p,q) (k-1) 

else ()

done;)

let time f =
  let t = Sys.time () in
  let res = f () in
  Printf.printf "Execution time: %f seconds\n"
    (Sys.time () -. t);
  res;;

let () = time(fun()-> recur (x,y) k);; 
Printf.printf "%d\n" !count