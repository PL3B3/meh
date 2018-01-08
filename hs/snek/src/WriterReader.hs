import Bfuck as B

removeNum :: (Eq a) => [a] -> a -> Int -> [a]
removeNum (x:xs) rm n
    | n == 0 = (x:xs)
    | xs == [] && x == rm = []
    | xs == [] && x /= rm = [x]
    | x == rm = removeNum xs rm (n - 1)
    | otherwise = x:(removeNum xs rm n)
    
makeLegit :: B.BFSequence -> String
makeLegit code
    | bias == 0 = code  
    | bias < 0 = removeNum code '[' (-(bias))
    | otherwise = reverse (removeNum (reverse code) ']' bias)
        where bias = B.checkBF'' (B.BFSequence $ parseBF code)