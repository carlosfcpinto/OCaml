type  image = White | Black |Node of image * image * image * image   (* node  with  fourchildren  *)

let node = function
| White, White, White, White -> White
| Black, Black, Black, Black -> Black
| q1,q2,q3,q4                -> Node (q1,q2,q3,q4)

(*let min_quad a b c d =
  min a b |> min c |> min d*)

let cutter = function
  2 -> 1
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
let why = read_line()

let size = Scanf.scanf("%d %d") (fun x z -> x)

let matrix = Array.make_matrix size size White

let rec reader i =
  if i = size then () else(
  let aux = read_line() in
  (*let aux  =  Scanf.scanf("%s") (fun x -> x) in*)
  let aux2 =  String.split_on_char ' ' aux in
  let iterator = (ref 0) in
  List.iter (fun z -> matrix.(i).(!iterator) <- (match z with "0" -> White |"1" -> Black |_ -> Black); iterator:=!iterator+1) aux2;
  (reader (i+1)))

let rec tree_builder x y size =
  if size = 1 then matrix.(x).(y) else
  node ((tree_builder x y (size/2)), (tree_builder (x + size/2) y (size/2)), (tree_builder (x + size/2) (y + size/2) (size/2)), (tree_builder x (y+size/2) (size/2)))

let rec leafcounter i = function
  Black | White -> (i,1)
  |Node (a, b, c, d) -> ((leafcounter (i+1) a) +++ (leafcounter (i+1) b) +++ (leafcounter (i+1) c) +++ (leafcounter (i+1) d))

let () = reader 0
let (a,b) = leafcounter 0 (tree_builder 0 0 size)
let () = Printf.printf ("%d\n%d\n") a b 