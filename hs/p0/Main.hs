succc x = succ (succ x)

fc x = product [1..x]

factors c = [a | a <- [1..c], mod c a == 0]

pqroots x = [(a, b)| a <- factors (last x), b <- factors (head x)]
