let p = 31                

let m = 1_000_000_000 + 9 

let code  c = Char.code c - Char.code 'a' + 1

let rolling_hash s =
  let z = String.length s in
  let i = ref 0 in
  let hash_val = ref 0 in
  let power_of_p = ref 1 in
  while (!i < z) do
    hash_val := ((!hash_val + (code (String.get s !i))) * !power_of_p) mod m;
    power_of_p := (!power_of_p * p) mod m;
    i := ! i + 1;
  done;
  !hash_val;;

let rec rolling_hash_rec (s: string) (i: int) (acc: int) (powerp: int) =
  if i =(String.length s) then acc else  let acc2 = (acc + (code (String.get s i) * powerp)) mod m in
    rolling_hash_rec s (i+1) acc2 ((powerp * p) mod m)