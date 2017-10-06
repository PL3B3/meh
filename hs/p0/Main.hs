succc x = succ (succ x)

fc x = product [1..x]

<<<<<<< HEAD
factors c = [a | a <- [1..c], mod c a == 0]

pqroots x = [(a, b)| a <- factors (last x), b <- factors (head x)]
=======
factors x = [a | a <- [1..x], mod x a == 0]

pq :: (Integral a) => [a] -> [Float]
pq x = [(fromIntegral a) / (fromIntegral b) | a <- factors (last x), b <- factors (head x)]

--syndiv :: (Integral a, Num b) => [a] -> b -> [b]
syndiv poly fac = foldl (\x y -> x ++ [y + ((last x) * fac)]) [head poly] (tail poly)

pqfacs x = [a | a <- pq x, last (syndiv x a) == 0.0]
>>>>>>> ff1676db13918b70c7dbc0d07893747c85fb7406
