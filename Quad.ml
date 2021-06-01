type  image = White | Black |Node of image * image * image * image   (* node  with  fourchildren  *)

let node = function
| White, White, White, White -> White
| Black, Black, Black, Black -> Black
| q1,q2,q3,q4                -> Node (q1,q2,q3,q4)

let why = Scanf.scanf("%s") (fun x -> x)

let size = Scanf.scanf("%d %d") (fun x z -> x)

let matrix = Array.make_matrix size size White

let rec reader i =
  if i = size then () else
  let aux  =  Scanf.scanf("%s") (fun x -> x) in
  let aux2 =  String.split_on_char ' ' aux in 
  let iterator = (ref 0) in
  List.iter (fun z -> matrix.(i).(!iterator) <- (match z with "0" -> White |"1" -> Black |_ -> Black); iterator:=!iterator+1) aux2;
  (reader (i+1))

