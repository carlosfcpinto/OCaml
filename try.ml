(*Autores:Carlos Francisco Caramelo Pinto, aluno nº39431
          Miguel Braga Santos, aluno nº 41541*)

(*Fonte:"Introdução à Programação Funcional em OCaml"
Mário Pereira and Simão Melo de Sousa, página 109
Função de memoização automática para uma função recursiva genérica
Função utilizada para optimizar o programa e evitar computações de uma valor já calculado
A função memo aceita a função alvo da memoização e devolve esta mesma função provida agora do mecanismo de memoização*)
let memo func =
  let table = Hashtbl.create 999 in
  let rec f v =
    try Hashtbl.find table v
    with Not_found ->
      let ret = func f v in
      Hashtbl.add table v ret;
      ret
  in
  f

(*Adaptação da função min dada pela biblioteca standard de OCaml para se tornar aplicável a argumentos do tipo option 'a
Usada na função guide para calcularmos o melhor caminho até chegarmos ao número 42*)
let min_opt x a =
  match (x, a) with
  | Some z, None -> Some z
  | None, Some z -> Some z
  | None, None -> None
  | Some z, Some x -> Some (min x z)

let (+++) x y = 
  match x, y with
    |z, Some w -> Some (z+w)
    |_ -> None

let regra1 x  = 
  match x mod 2 with
    |0 -> x - x/2
    |_ ->  0 
let regra2 x= 
  match min (x mod 3) (x mod 4) with
  |0 -> (let a = x - (x mod 10 * (x / 10 mod 10)) in
          if a <> x then a else 0)
  |_ -> 0
let regra3 x = 
  match x mod 5 with
  |0 -> x - 42
  |_ -> 0
(*Função principal deste problema, adaptada para ser alvo de memoização conforme os parâmetros da função acima explicada
O inteiro n representa o dinheiro que temos ainda em mão, sendo que os passos que damos são calculados automaticamente pela função, 
evitando assim parâmetros excessivos na chamada da mesma
Esta função devolverá um int option, para tratarmos dos casos em que não nos é possível chegar à soma de 42, 
sendo que a função deteta essa impossibilidade quando o nosso valor é menor que 42 devolvendo assim None, 
para qualquer outro caso devolverá Some x, sendo que x representa o número mínimo de passos dados para chegar a 42, ponto de paragem da função.
Usamos uma função de match para sabermos quais das regras podemos cumprir com o dinheiro que temos em mão,
chamando a função com a nova soma em mão depois de cumprida a regra e com um passo adicionado ao seu somatório*)
let galaxy = fun guide x ->
  if x < 42 then None
  else if x = 42 then Some 0
  else
    let a, b, c = (regra1 x), (regra2 x), (regra3 x) in
    1 +++ (guide a)
    |> min_opt (1 +++ guide b
                |> min_opt (1 +++ guide c))

(*Provemos aqui a nossa função galaxy do mecanismo de memoização, de modo a tornar a mesma mais eficiente*)
(*Leitura do input, não tratamos de edge cases, mas poderíamos pôr constraints para que este n estivesse entre 0 e 1 000 000*)
(*Cálculo do resultado *)
let () =
  let galaxym = memo galaxy in
  read_int () |> galaxym
  |> (function None -> "BAD LUCK" | Some z -> string_of_int z)
  |> print_endline

(*Impressão do resultado, fazendo um match ao resultado que sabemos ser um int option para que quando este seja None imprimirmos BAD LUCK,
sendo que em qualquer outro caso nos é devolvido Some z e passamos esse z que é um int para uma string para o imprimir no standard output
Não usamos aqui o \n, pois o mesmo já é provido pela função print_endline*)

(*Exemplo de execução, assumindo apenas o ramo que nos levará à solução:
galaxym 228 = 
  guide 228 = guide 228 (Regra 2) 1 passo
            + guide 212 (Regra 2) 1 passo
            + guide 210 (Regra 3) 1 passo
            + guide 168 (Regra 1) 1 passo
            + guide 84  (Regra 1) 1 passo
            + guide 42 -> Devolverá 0 pois é condição de paragem da função
  Devolverá Some 5, que será depois traduzido em 5, que são os 5 passos tomados para chegar ao valor 42
*)
