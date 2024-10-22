-- 4
poprawnaData rok miesiac dzien =
	let
		--poprawnyRok rok = rok > 0
		poprawnyRok = (> 0)
		poprawnyMiesiac miesiac = miesiac > 0 && miesiac < 13
		poprawnyDzien rok miesiac dzien
			| dzien < 1 || dzien > 31 = False
			| otherwise =
				(case miesiac of
					1 -> True
					3 -> True
					5 -> True
					7 -> True
					8 -> True
					10 -> True
					12 -> True
					2 -> dzien <= 28 || przestepny rok && dzien <= 29
					_ -> dzien <= 30)
			where
				przestepny rok
					| mod rok 4 /= 0   = False
					| mod rok 100 /= 0 = True
					| mod rok 400 /= 0 = False
					| otherwise = True
{--
		poprawnyDzien rok miesiac dzien
		| dzien < 1 || dzien > 31 = False
		| miesiac == 2 && dzien == 28 && rem rok 4 == 1 || (rem rok 100 == 0 && (rem rok 100 == 1 || rem rok 400 == 1)) = True
		| miesiac == 2 && dzien == 29 && rem rok 4 == 0 && (rem rok 100 == 1 || (rem rok 100 == 0 && rem rok 400 == 0)) = True
		| miesiac > 0 && miesiac < 8 && rem miesiac 2 == 0 && dzien == 30 = True
		| miesiac > 0 && miesiac < 8 && rem miesiac 2 == 1 && dzien == 31 = True
		| miesiac > 7 && miesiac < 13 && rem miesiac 2 == 0 && dzien == 30 = True
		| miesiac > 7 && miesiac < 13 && rem miesiac 2 == 1 && dzien == 31 = True
--}
	in
		poprawnyRok rok && poprawnyMiesiac miesiac &&
		poprawnyDzien rok miesiac dzien

-- 5

--f 1 = 1
--f n = 1 / n + f (n - 1)

f 0 a = a
f n a = f (n - 1 ) (a + 1 / n)

-- 7

--fib 1 = 1
--fib 2 = 2
--fib n = fib (n - 2) + fib (n - 1)

fib 1 a b = a
fib n a b = fib (n - 1) b (a + b)

-- 10

empty :: a -> Bool
empty _ = False

odd_numbers :: (Integral a) => a -> Bool
--odd_numbers x = rem x 2 == 1
--odd_numbers x = mod x 2 == 1
--odd_numbers x = odd x
odd_numbers = odd

even_numbers :: (Integral a) => a -> Bool
even_numbers = even

contains :: (a -> Bool) -> a -> Bool
--contains s el = s
