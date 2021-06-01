type suffix = { index:int ; stuff:string}

let string1 = read_line()
let string2 = read_line()
let first = String.length string1 + 1
let second = first

let sortsuffix (suffix1:suffix) (suffix2:suffix) =
  String.compare suffix1.stuff suffix2.stuff

let rec buildSuffixArray (x:string) (len:int) (suffixes:suffix array) = 
  match String.length x with
  |0 -> suffixes
  |_ -> let suffixes = Array.append suffixes (Array.make 1 {index = len - String.length x; stuff = x}) in
        let x = String.sub x 1 (String.length x-1) in
        buildSuffixArray x len suffixes

let t = string1 ^ "#" ^ string2 ^ "$"
let suffixarray = buildSuffixArray t (String.length t) [||]
let () = Array.fast_sort sortsuffix suffixarray


let check suffix1 suffix2 =
  if((suffix1.index < first && suffix2.index >= second) 
  || suffix1.index >= second && suffix2.index < first) 
  then (true) 
  else (false)

let rec suffixcompare i z max suffixarray = 
  if (check (Array.get suffixarray i) (Array.get suffixarray (i-1))) then(
  let c = String.get (Array.get suffixarray i).stuff z in
  let b = String.get (Array.get suffixarray (i-1)).stuff z in
  if (c=b) then (suffixcompare i (z+1) (max+1) suffixarray) else (max)) else (0)

(*i tem que começar como 1*)
let rec lcs (i:int) (suffixarray: suffix array) (matches: int array)= 
  match i with
  |x when (x = Array.length suffixarray) -> matches
  |_->let z = 0 in
    let max2 = suffixcompare i z 0 suffixarray in
    let matches = Array.append matches (Array.make 1 max2) in 
    lcs (i+1) suffixarray matches


let matches = (lcs 1 suffixarray [||])
let () = Array.fast_sort compare matches

let () = Printf.printf "%d\n" matches.(Array.length matches - 1)
