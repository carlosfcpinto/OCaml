open List
let n = read_int()
let k = read_int()
let a = read_int()
let b = read_int()

let movimentos = [(-2,-1)  ; (-2,+1) ;
                  (-1,-2)  ; (-1,+2) ;
                  (+1,-2)  ; (+1,+2) ;
                  (+2,-1)  ; (+2,+1)] ;; 

let tabuleiro (a,b) = (0<=a) && (a<n) && (0<=b) && (b<n) ;; 

let positions = [(0,0)] ;;
let cont = ref 0;; 
let add (a,b) (c,d) = (a+c,b+d);;

let rec move (a,b) = 
  if !cont >= k then 0 else (
  let _pos2 = (List.map (add (a,b)) movimentos) ;
  let _posl = (List.filter (tabuleiro) _pos2) ;
  let _positions = (positions @ _posl) ;
  cont := 1 + !cont ;
  let len = List.length posl ;
  for i=0 to len do
      move (List.nth posl i);
  done;
);;

let positions = List.sort_uniq compare positions ;
let len = List.length (positions) ;
Printf.printf ("%d") (len-1) ;
  



  (* if c==k break
  else 
  List.map position + List.iter movimentos
  if tabuleiro in List.iter position then k=k+1
  move c+1;; *)