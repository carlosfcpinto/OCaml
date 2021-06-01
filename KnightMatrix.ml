let moves=[|(1,2);(1,-2);(-1,2);(-1,-2);(2,1);(2,-1);(-2,1);(-2,-1)|];; (*Movimentos do cavalo*)

let verifica a b x y n = (a+x<n && b+y<n) && (0<=a+x && 0<=b+y) (*Verifica se a casa a estudar está no tabuleiro*)

let salto n a b matrix r = (*Realiza os possiveis saltos*)
    let aux = !matrix.(a).(b) in
        for i=0 to 7 do
            let (x, y) = moves.(i) in
                if verifica a b x y n then
                    !r.(a+x).(b+y) <- aux + !r.(a+x).(b+y)
                else ()
        done

let loopSalto n k matrix r = (*Realiza os saltos para cada ponto diferente para cada K*)
    for i=1 to k do
        Array.blit (Array.make_matrix n n 0) 0 !r 0 n;
        for h=0 to n-1 do 
            for j=0 to n-1 do
                if !matrix.(h).(j) == 0 then 
                    () 
                else
                    salto n h j matrix r
            done
        done;
        Array.blit !r 0 !matrix 0 n
    done

let somat = Array.fold_left (+) 0 (*Somatório de um array*)

exception InputInvalido

let valParametros n k a b = (*Verifica se os parametros de entrada estao dentro dos limites*)
    (n>0) && (n<=50) && (k>0) && (k<=8) && (a>=0) && (a<n) && (b>=0) && (b<n)

let () =
    let (n, k, a, b) = Scanf.scanf "%d %d %d %d\n" (fun n k a b -> (n, k, a, b)) in (*Entrada de parametros*)
        if valParametros n k a b then (
            let matrix = ref (Array.make_matrix n n 0) in (*Matrix principal*)
            let r = ref (Array.make_matrix n n 0) in (*Matrix Aux*)
                !matrix.(a).(b)<-1; (*Casa inicial*)
                loopSalto n k matrix r;
                let result = somat (Array.map somat !matrix) in (*Soma dos valores da Matrix*)
                    Printf.printf "%d\n" result
        ) else raise InputInvalido