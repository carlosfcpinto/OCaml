type  image = White | Black |Node of image * image * image * image   (* node  with  fourchildren  *)

let node = function
| White, White, White, White -> White
| Black, Black, Black, Black -> Black
| q1,q2,q3,q4                -> Node (q1,q2,q3,q4)

let (++++) a b =
  let (c,d) = a in
  let (e,f) = b in
  (c+e,d+f)

let rec numbernode size= function
| White -> ((size*size) ,0)
| Black -> (0,(size*size))
| Node (q1,q2,q3,q4) -> ((numbernode (size/2) q1) ++++ (numbernode (size/2) q2) 
                        ++++ (numbernode (size/2) q3) ++++ (numbernode (size/2) q4))

(*let maxcol = (fun (x, y, z, w) -> if((numbernode x + numbernode y + numbernode z + numbernode w) >= 0) then Black else White)*)
let node2 size = function
  |Node(q1,q2,q3,q4) -> ((*maxcol ((node2 q1), (node2 q2), (node2 q3), (node2 q4))*)
                        let (x,y) = ((numbernode size q1) ++++ (numbernode size q2) 
                        ++++ (numbernode size q3) ++++ (numbernode size q4)) in
                        if y>=x then Black
                        else White)
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
  if i = size then matrix else(
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

let rec tree_cutter cut i size = fun x ->
  if i = cut then node2 size x else
  match x with 
  |White -> White
  |Black -> Black
  |Node (q1, q2, q3, q4) -> let q5 = (tree_cutter cut (i+1) size q1) in
                            let q6 = (tree_cutter cut (i+1) size q2) in
                            let q7 = (tree_cutter cut (i+1) size q3) in
                            let q8 = (tree_cutter cut (i+1) size q4) in
                            node (q5, q6, q7, q8)

let rec leafcounter i = function
  Black | White -> (i,1)
  |Node (a, b, c, d) -> ((leafcounter (i+1) a) +++ (leafcounter (i+1) b) +++ (leafcounter (i+1) c) +++ (leafcounter (i+1) d))

let why = read_line()
let size = List.hd (read_line() |> String.split_on_char ' ' |> List.map (fun x -> int_of_string x))
let matrix = Array.make_matrix size size White
let matrix_in = reader 0 size matrix
let tree = tree_builder 0 0 size matrix_in
let (a,b) = leafcounter 0 tree
let size_out = read_line() |> int_of_string
let cut = cutter size_out
let tree_cut = tree_cutter cut 0 size tree 
let matrix_out = Array.make_matrix size_out size_out 0
let () =matrix_builder 0 0 (size_out) matrix_out tree_cut; 
        print_endline (string_of_int a); print_endline (string_of_int b);
        for i=0 to (Array.length matrix_out.(0))-1 do
            for j=0 to (Array.length matrix_out.(0))-1 do
              if j = ((Array.length matrix_out.(0))-1) then
                Printf.printf ("%d") matrix_out.(i).(j)
              else
                Printf.printf ("%d ") matrix_out.(i).(j)
            done;
            print_newline();
          done