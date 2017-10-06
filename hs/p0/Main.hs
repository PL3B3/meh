succc x = succ (succ x)

fc x = product [1..x]

factors c = [a | a <- [1..c], mod c a == 0]

pqroots x = [(a, b)| a <- factors (last x), b <- factors (head x)]


pq :: (Integral a) => [a] -> [Float]
pq x = [(fromIntegral a) / (fromIntegral b) | a <- factors (last x), b <- factors (head x)]

--syndiv :: (Integral a, Num b) => [a] -> b -> [b]
syndiv poly fac = foldl (\x y -> x ++ [y + ((last x) * fac)]) [head poly] (tail poly)

pqfacs x = [a | a <- pq x, last (syndiv x a) == 0.0]
