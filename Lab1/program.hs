-- komentarz

{--
Komentarz blokowy
--}

s x = x + 1

add x y = x + y
add' = (+)
sub x y = x - y
add6 y = 6 + y
add5 = add 5
-- add5 6
-- (add 5) 6
-- add 5 6
-- 5 + 6

signum' x =
    if x == 0 then 0
    else
        if x < 0 then -1
        else 1

s' 0 = 0
s' x = 1

f 0 0 = 0
f x 0 = x + 5
f 1 y = y + 10
f x y = x + y

-- head' [x] = x -- to jest redundant

head' (h:_) = h -- to mogło by być tail, ale nas nie obchodzi, bo nie jest to od tego zależne
last' [x] = x
last' (_:t) = last' t

length' [] = 0
length' (_:t) = 1 + length' t -- bo liczymy ile jest tam ogonów w tej funkcji i dodajemy 1 za głowę

