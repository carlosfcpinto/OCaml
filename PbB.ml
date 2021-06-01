type quad = White | Black | Node of quad * quad * quad * quad

let node = function
| White, White, White, White -> White
| Black, Black, Black, Black -> Black
| q1,q2,q3,q4 -> Node (q1,q2,q3,q4)















let why = read_line ()

let x,y  = Scanf.scanf "%d %d" (fun z w -> (z, w))

let depth = read_int()


