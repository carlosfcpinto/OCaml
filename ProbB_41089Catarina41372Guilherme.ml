(*Autoria: Catarina Capinha, a41089 e Guilherme OLiveira, a41372*)

(*Websites utilizados:
  1---  https://stackoverflow.com/questions/51033803/recursive-solution-to-common-longest-substring-between-two-strings
  2---  https://www.geeksforgeeks.org/longest-common-substring-dp-29/
  3---  https://caml.inria.fr/pub/docs/oreilly-book/html/book-ora027.html
  4---  https://www.geeksforgeeks.org/hamming-distance-two-strings/
  5---  https://stackoverflow.com/questions/19158025/longest-common-substring-in-two-sequences-of-strings
  6---  https://asecuritysite.com/calculators/ham?a=101101010&b=011101100
  7---  https://github.com/lpil/exercism/tree/master/ocaml/hamming
  8---  https://exercism.io/tracks/ocaml/exercises/hamming/solutions/4deb1634f97345ff8719497010f073a1
  9---  https://stackoverflow.com/questions/43403960/hamming-distance
 10---  https://exercism.io/tracks/ocaml/exercises/hamming/solutions/e4cbc94e907a48378c77e8a601fd450f
 11---  https://www.geeksforgeeks.org/kmp-algorithm-for-pattern-searching/
 12---  https://discuss.ocaml.org/t/write-a-function/3613/4
 13---  https://repositorio.ufba.br/ri/bitstream/ri/20213/1/LCS_TCC-Final.pdf
 14---  http://createsoftware.users.sourceforge.net/articles/Sorting%20in%20OCaml%20-%20C.%20Pit--Claudel.pdf
 15---  http://www.integrade.org.br/files/jai2007-texto.pdf
 16---  https://ocaml.org/learn/tutorials/if_statements_loops_and_recursion.html
 17---  https://www.geeksforgeeks.org/check-whether-two-strings-contain-same-characters-in-same-order/
 18---  https://www.geeksforgeeks.org/count-number-of-increasing-sub-sequences-onlogn/?ref=leftbar-rightbar
 19---  https://www.cs.princeton.edu/~dpw/courses/cos326-12/notes/parallel-complexity.php
 20---  https://discuss.ocaml.org/t/write-a-function/3613
 21---  https://pt.stackoverflow.com/questions/56836/definição-da-notação-big-o
 22---  https://pt.wikipedia.org/wiki/Quicksort
 23---  https://caml.inria.fr/pub/docs/manual-ocaml/libref/String.html 
 24---  https://stackoverflow.com/questions/1592649/examples-of-algorithms-which-has-o1-on-log-n-and-olog-n-complexities
 25---  https://en.wikipedia.org/wiki/Longest_common_subsequence_problem
 26---  https://www.pythonlearn.com/html-008/cfbook007.html
 27---  https://stackoverflow.com/questions/14032903/longest-common-contiguous-subsequence-algorithm
 28---  https://stackoverflow.com/questions/10248728/how-to-find-longest-common-substring-using-c
 29---  https://www.geeksforgeeks.org/longest-common-substring-dp-29/
 30---  https://algorithms.tutorialhorizon.com/dynamic-programming-longest-common-substring/ *)

let n = 5000

let lista1 = String.uppercase_ascii (read_line()) 
let lista2 = String.uppercase_ascii (read_line())
 
 (* Hard coded value to debug more quickly 
 let lista1 = "ATTGCAG"
 let lista2 = "CTAGG" and the result is 2*)
 
let tam_lista1 = String.length lista1
let tam_lista2 = String.length lista2
 
let rec confirma lista tam_lista = (*Função criada para sabermos se as strings contêm apenas as letras correspondentes aos aminoácidos*)
   if tam_lista = 0 then true else
   match lista.[tam_lista-1] with
   |'A' |'C' |'G' |'T' -> confirma lista (tam_lista-1) 
   |_ -> false

let rec distancia (lista1:string) (tam_lista1:int) (lista2:string) (tam_lista2:int) confirma =
   match lista1, lista2 with
   | lista1, "" -> 0 (*Se uma das listas estiver vazia então retorna zero*)
   | "" , lista2-> 0 (*Se uma das listas estiver vazia então retorna zero*)
   | lista1, lista2 when (confirma lista1 tam_lista1 && confirma lista2 tam_lista2)-> 
                           let matriz = Array.make_matrix (tam_lista1 + 1) (tam_lista2 + 1) 0 in
                           let count1 = ref 0 in (
                           for i = 0 to tam_lista1 do
                              for j = 0 to tam_lista2 do

                                 if (i = 0 || j = 0) then (*Se uma das listas ou ambas forem vazias então elas irão retornar zero porque é preciso que ambas sejam válidas*)
                                    matriz.(i).(j) <- 0

                                 else if lista1.[i-1] = lista2.[j-1] then(
                                    let() = matriz.(i).(j) <- matriz.(i-1).(j-1) + 1 in (*Percorrendo as duas strings por forma matricial vamos ver se o conteúdo das posições são iguais e caso positivo então continuamos com a incrementação*)
                                           count1 := max !count1 matriz.(i).(j) ) (*Será feita a avaliação do máximo valor encontrado da maior substring, assim poderemos fazer o produto deste tópico com a avaliação das strings a partir da posição inicial*)
                                 
                                 else
                                    matriz.(i).(j) <- 0 (*Caso não sejam iguais então tomamos como zero*)
            
                              done
                           done;
                           !count1 )        
   | _ -> failwith "This case is impossible"
 
let () = print_int ((distancia lista1 tam_lista1 lista2 tam_lista2 confirma))
let () = print_endline ""