fib 0 = 0
fib 1 = 1
fib n = fib(n - 2) + fib(n - 1)

-- przejście do kolejnego kroku to wywołanie kolejne tej samej funkcji z innymi parametrami
-- liczniki przechowujemy jako parametry funkcji

fib' 0 = 0
fib' n = fib_iter n 1 0 1
fib_iter n i prev fib_i =
    if i == n then fib_i
    else fib_iter n (i+1) fib_i (prev + fib_i)

sin' x = sin_iter x 1 x x 
sin_iter x i s a = 
    if (abs a) < 0.0000001 then s
    else
        let
            el = -a * x^2 / ((i + 1) * (i + 2))
        in 
            sin_iter x (i + 2) (s + el) el

cos' x = sin_iter x 0 1 1 

bisekcja a = bisekcja_iter a 0 a

bisekcja_iter a lewy prawy = 
    let
        srodek = (lewy + prawy) * 0.5 
    in  -- pętla
        if prawy - lewy < 0.00000001 then
            srodek
        else
            if srodek ^ 2 - a < 0 then
                bisekcja_iter a srodek prawy
            else
                bisekcja_iter a lewy srodek


