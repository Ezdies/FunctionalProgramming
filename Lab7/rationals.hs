data Fraction = Integer :/ Integer -- operatory które są konstruktowami muszą zaczynać się od dwukropka
-- Eq porównuje pierwsze elementy najpierw i już po pierwszych stwierdza czy coś jest mniejsze czy większe


infixl 7 :/

--to jest po prostu funcja którą można stosować jako
(//) :: Integer -> Integer -> Fraction
p // q = norm (p :/ q)

reduce :: Fraction -> Fraction
reduce  (p :/ q) = 
    let gcd_pq = gcd p q
    in 
        div p gcd_pq :/ div q gcd_pq

normSigns :: Fraction -> Fraction
normSigns(p :/ q)
    | q < 0 = (-p) :/ (-q)
    | otherwise = p :/ q

norm :: Fraction -> Fraction
norm (0 :/ _) = 0 :/ 1 --dopasowanie wzorca
norm frac = reduce(normSigns frac) 

nominator :: Fraction -> Integer
nominator (p :/ _) = p

denominator :: Fraction -> Integer
denominator (_ :/ q) = q

instance Num Fraction where
    fromInteger n = n :/ 1
    abs (p :/ q)= abs p // q
  --  signum = signum p :/ 1
    signum (0 :/ q) = 0 :/ 1
    signum (p :/ q)
        |signum p == signum q = 1 :/ 1
        |otherwise             = (-1) :/ 1
    negate (p :/ q) = negate p // q
    (p1 :/ q1) + (p2 :/ q2) = (p1 * q2 + p2 * q1) // (q1 * q2) 
    (p1 :/ q1) * (p2 :/ q2) = (p1 * p2) // (q1 * q2)

instance Show Fraction where
    show frac =
        let 
            p :/ q = norm frac
        in
            show p ++ "/" ++ show q

instance Eq Fraction where
    -- frac1 == frac2 = 
    --     let 
    --         p1 :/ q1 = norm frac1
    --         p2 :/ q2 = norm frac2
    --     in
    --         p1 == p2 && q1 == q2 
        (p1 :/ q1) == (p2 :/ q2) = p1 * q2 == q1 * p2
    
        

instance Ord Fraction where
    frac1 <= frac2 = 
        let 
            p1 :/ q1 = norm frac1
            p2 :/ q2 = norm frac2
        in p1 * q2 <= p2 * q1





f :: Num a => a -> a -> a
f x y = (x - y)^2

--definiowane własnych operatorów
-- jak chcemy robić infixowo czyli normalnie, to można korzystać z backticków ``

x # y = (x - y)^2 --operator gdzie jest hash jako symbol tej funkcji

--łączność jest ważna przy operatorach - minus jest łączny lewostronnie, a dwuktopek łączny prawostronnie
--łączność można sprawdzić w konsoli robiąc np :i (+)
infixl 6 # -- ustawiamy dla naszego operatora hasha priorytet i czy jest łącznie lewo czy prawostronnie
-- jak są dwa operatory z jeden lewostronny, a drugi prawostronny i mają taki sam priorytet, to będzie BŁĄD

