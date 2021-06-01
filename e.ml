let w = Scanf.scanf "%d\n" (fun x -> x)

let rec read part n =
  if n = 0 then part else 
  let z = Scanf.scanf "%d %d\n" (fun x z-> (z/x))(*read_line() |> int_of_string*) in read  (z::part) (n-1)

let () = w
        |> read []
        |> List.sort compare
        |> List.hd
        |> Printf.printf "%d\n"