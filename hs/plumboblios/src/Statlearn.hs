module Statlearn
    (printboi
	) where 

import StatBlob as SB
import Data.List as L

data Hypercube = Hypercube [Int] [Float] deriving Show 

testtrilst :: [(Float, Float, Int)]
testtrilst = [(10.3,20.4,1),(14.6,11.2, 1),(22.9, 15.6, 1),(2.2,3.1,0),(1.1,2.2,0),(8.3,6.5,0),(2.5,1.3,0),(25.6,22.4,1),(16.4,19.0,1),(8.3,1.2,0),(3.1,9.2,0),(23.1,22.1,1),(3.3,3.3,0),(12.2,18.2,1),(1.1,1.1,0),(5.6,1.1,0),(11.3,19.0,1),(2.2,0.3,0),(56.3,27.3,1),(12.3,15.3,1),(9.7,7.8,0),(11.2,31.0,1),(20.9,11.3, 1),(14.3,12.4,1),(19.1,19.1,1),(6.5,3.4,0),(12.1,3.3,0),(6.7,8.9,0),(6.7,14.8,1),(1.1,2.2,1),(22.5,12.3,0),(8.9,13.2,1),(4.5,19.2,0),(18.3,19.2,1),(8.3,3.3,0),(2.1,3.1,0),(10.0,10.0,1),(10.0,9.3,0)]

makeacube :: [Int] -> [Float] -> Hypercube
makeacube lsint lsflt = (Hypercube lsint lsflt)

printboi :: IO ()
printboi = putStrLn "YOU ARE BAD"

--coord :: [Int] -> Hypercube -> Float
--coord coords (Hypercube lsint lsflt) = lsflt !! (Product coords)  

mse :: [Float] -> [[Float]] -> ([Float] -> Float) -> Float
mse y x fhat = (1 / (fromIntegral (length y))) * (sum $ zipWith (\x y -> (x - y)^2) y (map fhat x))


knn :: [(Float, Float, Int)] -> (Float, Float) -> Int -> Int
knn trlst (a2, b2) nbors = mostcommon $ map snd (filter (\a -> L.elem (fst a) lowestnbors) distvalpairs)
    where lowestnbors = take nbors $ sort $ map fst distvalpairs  
          distvalpairs = map (\x@(a, b, c) -> ((sqrt $ (a2 - a)^2 + (b2 - b)^2), c)) trlst

mostcommon (x:xs)
    | count x (x:xs) == maxls (counts (x:xs)) = x
    | otherwise = mostcommon (filter (/=x) xs)
        where count x xl = (length $ filter (==x) xl)
              counts xl = foldl (\a x -> a ++ [count x xl]) [] xl

maxls (x:xs)
    | xs == [] = x
    | otherwise = max x (maxls xs)
{-
takelowest int (x:xs) = takelowest' int [] (x:xs)
    where   takelowest' int non (a:as)
                | int == 0 = []
                | as == [] = [a]
                | (fst x) > (fst (fst xs)) = takelowest' int (non ++ [fst xs]) (x:(tail xs))
                | 

flexibilities :: [Float]
    denote the flexibility of the model, within a certain range
data :: Hypercube
-}
--minmse :: [Float] -> 
--minmse  
{-
processCube :: Hypercube -> String
processCube (Hypercube lsint lsflt) = show matrixed
    where matrixed = foldl (\l f -> f l) lsflt (map (\n -> split n) lsint)
            where split n l 
                    | length l <= (fromIntegral n) = [(take n l)]
                    | otherwise = [(take n l)] ++ (split (n) (drop n l))  
-}


 