let size = Scanf.sscanf (read_line()) "%d"(fun size -> (size))
let form = Stack.create()

let rec verform x  = 
  let s = (try Stack.pop (form) 
           with |Stack.Empty -> 0) in
  if(s==0)then(if(x==3 || x==1 || x== -1)then(Printf.printf"YES\n") 
  else (Printf.printf"NO\n"))
  else(
  match x with 
  |0   -> (verform s)
  | -3 -> if(s==1 || s== -3) then (verform s) else (Printf.printf"NO\n")
  |1   -> if(s==1 || s== -3) then (Printf.printf"NO\n") else (verform s)
  |2   -> if(s==1 || s== -3) then (verform s) else (Printf.printf"NO\n")
  | -1 -> if(s==1 || s== -3) then (Printf.printf"NO\n") else (verform s)
  |3   -> if(s==2 || s== -1 || s==3) then (verform s) else (Printf.printf"NO\n")
  |_   -> (Printf.printf"NO\n"))

let strtoint str =
  match str with 
  |"(" -> 3
  |")" -> -3
  |"->"|"&"|"|"|"<->" -> 2
  |"!" -> -1
  |_ -> 1

let rec contpar stcp size=
  if size == 0 then (+0) else(
  let s = Stack.pop stcp in
  if (s = 3) then (contpar stcp (size-1) + s) else
  (if(s = - 3) then (contpar stcp (size-1) + s) 
  else (contpar stcp (size-1) + 0)))

let rec inp z = 
  if z==0 then () else(
    let s = read_line() in
    let x = strtoint s in
    Stack.push x form;
    inp (z-1))

let () = inp size
let stcp = Stack.copy form
let () = if(contpar stcp size == 0) then (verform 0) else (Printf.printf"NO\n")