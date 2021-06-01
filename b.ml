let (n, x) = Scanf.scanf "%d %d\n" (fun x z -> (x,z))

let rec read n count =
  if n = 0 then List.stable_sort compare count else 
  let z = Scanf.scanf "%d\n" (fun x -> x)(*read_line() |> int_of_string*) in read (n-1) (z::count)

let rec sum n values =
  if n = 0 then 0 else
  match values with
    |x::xs -> x + (sum (n-1) xs)
    |[] -> 0

let () = read n [] |> List.rev |> sum x (*|> string_of_int*) |> Printf.printf "%d\n"