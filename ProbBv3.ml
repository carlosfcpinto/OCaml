(*Algoritmo usado após consulta das páginas 
  http://web.stanford.edu/class/cs97si/suffix-array.pdf
  https://cp-algorithms.com/string/suffix-array.html
  https://www.youtube.com/watch?v=53VIWj8ksyI*)

(*Criamos um tipo, similar a struct em C para armazenar os sufixos de ambas as strings concatenadas num array de sufixos*)
type suffix = {index:int ; stuff:string}

(*Inputs, usamos o string.length para podermos navegar mais facilmente nos indexs dos sufixos*)
let string1 = read_line()
let string2 = read_line()
let first = String.length string1 + 1
let second = first

(*Função de comparação usada no sort, aplicando o String.compare apenas aos parâmetros do tipo sufixo onde isto é permitido*)
let sortsuffix (suffix1:suffix) (suffix2:suffix) =
  String.compare suffix1.stuff suffix2.stuff

(*Construção do array de sufixos, iteramos pela string que consiste nas duas strings concatenadas com dois carateres que são 
lexicograficamente inferiores a qualquer char que esteja na string, isto serviria para construir o array de sufixos com "cyclic shifts" 
mas acabamos por usar outra implementação, mais simples, mas admitidamente menos eficaz*)
let rec buildSuffixArray (x:string) (len:int) (suffixes:suffix array) =
  match String.length x with
  |0 -> suffixes
  |_ -> let suffixes = Array.append suffixes (Array.make 1 {index = len - String.length x; stuff = x}) in
        let x = String.sub x 1 (String.length x-1) in
        buildSuffixArray x len suffixes

let t = string1 ^ "#" ^ string2 ^ "$"
let suffixarray = buildSuffixArray t (String.length t) [||]
(*Ordenamos o array de sufixos construido previamente, usando um algoritmo de sorting implementado no módulo Array de Ocaml
Usa o Heap Sort e tem uma complexidade temporal de O(n Logn)*)
let () = Array.sort sortsuffix suffixarray

(*usamos esta função para nos certificarmos que comparamos apenas sufixos que pertençam a strings diferentes, 
comparações dentro da mesma string não nos interessam*)
let check suffix1 suffix2 =
  if((suffix1.index < first && suffix2.index >= second) 
  || suffix1.index >= second && suffix2.index < first) 
  then (true) 
  else (false)

(*iteramos pelos dois sufixos que passamos a esta função construindo a tabela de LCP, ou longest common prefix, 
devolvendo um inteiro que é de seguida inserido num array auxiliar do array de sufixos*)
let rec suffixcompare i z max suffixarray = 
  if (check (Array.get suffixarray i) (Array.get suffixarray (i-1))) then(
  let c = String.get (Array.get suffixarray i).stuff z in
  let b = String.get (Array.get suffixarray (i-1)).stuff z in
  if (c=b) then (suffixcompare i (z+1) (max+1) suffixarray) else (max)) else (0)

(*i tem que começar como 1, sendo que na verdade podia começar como 3 pois os dois primeiros elementos do array de sufixos são aqueles iniciados pelos símbolos $ e #
Dentro desta função chamamos a nossa função suffix compare e criamos o array dos LCP que usaremos para obter a nossa resposta final*)
let rec lcs (i:int) (suffixarray: suffix array) (matches: int array)= 
  match i with
  |x when (x = Array.length suffixarray) -> matches
  |_->let z = 0 in
    let max2 = suffixcompare i z 0 suffixarray in
    let matches = Array.append matches (Array.make 1 max2) in 
    lcs (i+1) suffixarray matches


let matches = (lcs 1 suffixarray [||])
(*Usamos mais uma vez a função implementada em Ocaml de sorting para acedermos apenas ao máximo elemento do array de LCP*)
let () = Array.sort compare matches

(*Devolvemos aquele que é o LCP*)
let () = Printf.printf "%d\n" matches.(Array.length matches - 1)

(*Exemplos de execução

  Os 16 segundos são apenas porque o input é feito de forma manual

  ACGTAGAGAGCGAGA
  GTGCGAGCGAGAGA
  8
  ./a.out  0.00s user 0.00s system 0% cpu 16.371 total


  Com duas Strings de ADN aleatoriamente geradas com uma length de 5000
  
  11
  ./a.out < teste.txt  0.45s user 0.12s system 98% cpu 0.572 total
*)