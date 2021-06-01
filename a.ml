let (n, x) = Scanf.scanf "%d %d" (fun x z -> (x,z))

let rec read n count =
  if n = 0 then List.fast_sort compare count else 
  let z = read_line() |> int_of_string in read (n-1) (z::count)

let rec sum n values =
  if n = 0 then 0 else
  match values with
    |x::xs -> x + (sum (n-1) xs)
    |[] -> 0

let () = read n [] |> sum x |> string_of_int |> print_endline