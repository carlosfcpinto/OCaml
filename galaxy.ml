(*let memo = 


let rec try42 i n =
  if i < 42 then max_int else
  let z = compare i 42 in
  match z with 
    |0 -> n
    |_ -> match i mod 2 with
          |0 -> (try42 (i/2) (n+1))
          |_ -> match i mod 5 with
                |0 -> (try42 (i-42) (n+1))
                |_ -> match (i mod 3, i mod 4) with
                      |(_,0)-> (try42 (i-((i mod 10) * (i mod 10) mod 10)) n+1)
                      |(0,_)-> (try42 (i-((i mod 10) * (i mod 10) mod 10)) n+1)
                      |(_,_)-> max_int*)

let divides x i = match x mod i with 0 -> true | _ -> false

let rec try42_2 i n =
  if i = 42 then n
  else if i < 42 then 0
  else
    match (divides i 2, divides i 3 || divides i 4, divides i 5) with
    | true, true, true ->
        min
          (min (try42_2 (i / 2) n + 1) (try42_2 (i - 42) n + 1))
          (try42_2 (i - (i mod 10 * (i / 10 mod 10))) n + 1)
    | true, false, true -> min (try42_2 (i / 2) n + 1) (try42_2 (i - 42) n + 1)
    | true, false, false -> try42_2 (i / 2) n + 1
    | true, true, false ->
        min
          (try42_2 (i / 2) n + 1)
          (try42_2 (i - (i mod 10 * (i / 10 mod 10))) n + 1)
    | false, true, true ->
        min
          (try42_2 (i - (i mod 10 * (i / 10 mod 10))) n + 1)
          (try42_2 (i - 42) n + 1)
    | false, false, true -> try42_2 (i - 42) n + 1
    | false, true, false -> try42_2 (i - (i mod 10 * (i / 10 mod 10))) n + 1
    | false, false, false -> max_int

let x = read_int ()

let () = Printf.printf "%d\n" (try42_2 x 0)
