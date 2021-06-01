let aux = read_line() 
          |> String.split_on_char ' ' 
          |> List.map int_of_string

let n, x = List.hd aux, List.hd(List.tl aux)

let hash = Hashtbl.create 99