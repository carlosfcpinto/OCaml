let print_triangle n =
  let x = ref 1 in 
  for i = 1 to n do
    for j = 1 to i+1 do
      if (j = i+1) then Printf.printf("\n") 
      else (Printf.printf ("%4d") !x;
      x := !x +1) 
    done
  done


let () = print_triangle (read_line() |> int_of_string)