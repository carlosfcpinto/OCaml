type  image = White | Black |Node of image * image * image * image   (* node  with  fourchildren  *)

let node = function
| White, White, White, White -> White
| Black, Black, Black, Black -> Black
| q1,q2,q3,q4                -> Node (q1,q2,q3,q4)

let numbernode = function
| White -> -1
| Black -> 1
|_ -> 0

let maxcol = (fun (x, y, z, w) -> if((numbernode x + numbernode y + numbernode z + numbernode w) >= 0) then Black else White)
let rec node2 = function
  |Node(q1,q2,q3,q4) -> maxcol ((node2 q1), (node2 q2), (node2 q3), (node2 q4))
  |Black -> Black
  |White -> White

(*let min_quad a b c d =
  min a b |> min c |> min d*)

let cutter = function
  |1 -> 0
  |2 -> 1
  |4 -> 2
  |8 -> 3
  |16 -> 4
  |32 -> 5
  |64 -> 6
  |128 -> 7
  |256 -> 8
  |512 -> 9
  |1024 -> 10
  |_ -> 0

let (+++) a b =
  let (c,d) = a in
  let (e,f) = b in
  ((min c e),d+f)

let rec reader i size matrix =
  if i = size then () else(
  let aux = read_line() in
  (*let aux  =  Scanf.scanf("%s") (fun x -> x) in*)
  let aux2 =  String.split_on_char ' ' aux in
  let iterator = (ref 0) in
  List.iter (fun z -> matrix.(i).(!iterator) <- (match z with "0" -> White |"1" -> Black |_ -> Black); iterator:=!iterator+1) aux2;
  (reader (i+1) size matrix))

let rec tree_builder x y size matrix =
  if size = 1 then matrix.(x).(y) else
  node ((tree_builder x y (size/2) matrix), (tree_builder (x + size/2) y (size/2) matrix), 
  (tree_builder (x + size/2) (y + size/2) (size/2) matrix), (tree_builder x (y+size/2) (size/2) matrix))

let rec matrix_builder x y size_out matrix_out = function
  White -> for i = 0 to (size_out-1) do
    matrix_out.(x+i).(y) <- 0;
    for j = 0 to (size_out-1) do
      matrix_out.(x+i).(y+j)<- 0
    done;
  done;
  |Black -> for i = 0 to (size_out-1) do
    matrix_out.(x+i).(y) <- 1;
    for j = 0 to (size_out-1) do
      matrix_out.(x+i).(y+j)<- 1
    done;
  done;
  |Node (q1, q2, q3, q4) -> (matrix_builder x y (size_out/2) matrix_out q1); 
                            (matrix_builder (x + size_out/2) y (size_out/2) matrix_out q2);
                            (matrix_builder (x + size_out/2) (y + size_out/2) (size_out/2) matrix_out q3);
                            (matrix_builder x (y+size_out/2) (size_out/2) matrix_out q4)

let rec tree_cutter cut i = fun x ->
  if i = cut then node2 x else
  match x with 
  |White -> White
  |Black -> Black
  |Node (q1, q2, q3, q4) -> let q5 = (tree_cutter cut (i+1) q1) in
                            let q6 = (tree_cutter cut (i+1) q2) in
                            let q7 = (tree_cutter cut (i+1) q3) in
                            let q8 = (tree_cutter cut (i+1) q4) in
                            Node (q5, q6, q7, q8)

let rec leafcounter i = function
  Black | White -> (i,1)
  |Node (a, b, c, d) -> ((leafcounter (i+1) a) +++ (leafcounter (i+1) b) +++ (leafcounter (i+1) c) +++ (leafcounter (i+1) d))


let why = read_line()

let size = Scanf.scanf("%d %d") (fun x z -> x)

let matrix = Array.make_matrix size size White

let () = reader 0 size matrix
let tree = tree_builder 0 0 size matrix
let (a,b) = leafcounter 0 tree
let size_out = read_int ()

let cut = cutter size_out
let tree_cut = tree_cutter cut 0 tree
let matrix_out = Array.make_matrix size_out size_out 0
let () = matrix_builder 0 0 (size_out) matrix_out tree_cut 
let () = Printf.printf ("%d\n%d\n") a b
let () = for i = 0 to (size_out-1) do
        for j = 0 to (size_out-1) do
        Printf.printf "%d " matrix_out.(i).(j)
        done;
        Printf.printf "\n"
      done;