import Data.List

scramble :: [String]
scramble = cycluq (quot (length list) 2) pairlist
  where pairlist = foldl (\x y -> if (elem (reverse y) x) then x else (x ++ [y])) [] (pairs list)
        pairs [] = []
        pairs (x:xs) = (zipWith (\x y -> [x,y]) (take (length xs) $ repeat x) xs) ++ pairs xs
        nosect a b = not $ any (\x -> elem x b) a   
        list = "abcdefgh"

cycluq num list
    | length list < num = uqfromprevs (length list) [] list
    | otherwise = uqs ++ (cycluq num (deletels uqs list))
        where uqs = uqfromprevs num [] list 
              deletels rm ls = foldl (\x y -> delete y x) ls rm

uqfromprevs num sorted (x:xs)
    | length sorted == num = []
    | sorted == [] = [x] ++ (uqfromprevs num [x] xs)
    | otherwise = [uq] ++ (uqfromprevs num (sorted ++ [uq]) (delete uq (x:xs)))
        where uq = head (filter (\a -> nosectls a sorted) (x:xs))
              nosectls a ls = not $ any (\x -> elem x (concat ls)) a