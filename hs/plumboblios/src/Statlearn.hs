module Statlearn (printboi) where 

import StatBlob as SB
import Data.List as L

--Int is dimensions, from highest to lowest level
data Hypercube = Hypercube [Int] [Float] deriving Show 

xpec = (\x -> sum x / (fromIntegral $ length x))

testtrilst :: [(Float, Float, Int)]
testtrilst = [(10.3,20.4,1),(14.6,11.2, 1),(22.9, 15.6, 1),(2.2,3.1,0),(1.1,2.2,0),(8.3,6.5,0),(2.5,1.3,0),(25.6,22.4,1),(16.4,19.0,1),(8.3,1.2,0),(3.1,9.2,0),(23.1,22.1,1),(3.3,3.3,0),(12.2,18.2,1),(1.1,1.1,0),(5.6,1.1,0),(11.3,19.0,1),(2.2,0.3,0),(56.3,27.3,1),(12.3,15.3,1),(9.7,7.8,0),(11.2,31.0,1),(20.9,11.3, 1),(14.3,12.4,1),(19.1,19.1,1),(6.5,3.4,0),(12.1,3.3,0),(6.7,8.9,0),(6.7,14.8,1),(1.1,2.2,1),(22.5,12.3,0),(8.9,13.2,1),(4.5,19.2,0),(18.3,19.2,1),(8.3,3.3,0),(2.1,3.1,0),(10.0,10.0,1),(10.0,9.3,0)]

testptslst = map (\x@(a,b,c) -> ([a,b],c)) testtrilst

testcube = makeacube [3,3,2] [3.3,4.4,1.2,6.5,8.7,2.6,10.3,2.5,7.6,19.3,1.8,11.5,3.8,1.0,0.5,2.6,78.1, 9.4]

makeacube :: [Int] -> [Float] -> Hypercube
makeacube lsint lsflt = (Hypercube lsint lsflt)

printboi :: IO ()
printboi = putStrLn "YOU ARE BAD"

coord' :: [Int] -> Hypercube -> Float
coord' coords (Hypercube lsint lsflt) = lsflt !! sum (foldl (\b a -> b ++ [(a * (div (length lsflt) (product $ take (succ $ length b) lsint)))]) [] coords)

coord :: [Int] -> Hypercube -> [Float]
coord coords (Hypercube lsint lsflt) = take (div (length lsflt) (product (take (length startposit) lsint))) (drop ((sum startposit) - 1) lsflt)  
        where startposit = (foldl (\b a -> b ++ [(a * (div (length lsflt) (product $ take (succ $ length b) lsint)))]) [] coords)



--frame :: [Int] -> Hypercube -> [Float]
--frame coords (Hypercube lsint lsflt) =              

mse :: [Float] -> [[Float]] -> ([Float] -> Float) -> Float
mse y x fhat = (1 / (fromIntegral (length y))) * (sum $ zipWith (\x y -> (x - y)^2) y (map fhat x))

--basic k nearest neighbors algorithm
knn :: [(Float, Float, Int)] -> (Float, Float) -> Int -> Int
knn trlst (a2, b2) nbors = mostcommon $ map snd (filter (\a -> L.elem (fst a) lowestnbors) distvalpairs)
    where lowestnbors = take nbors $ sort $ map fst distvalpairs  
          distvalpairs = map (\x@(a, b, c) -> ((sqrt $ (a2 - a)^2 + (b2 - b)^2), c)) trlst

--like knn but works for any number of dimensions (hopefully)
knn' :: [([Float], Int)] -> [Float] -> Int -> Int
knn' pntlst pnt nbr = mostcommon $ map snd (filter (\a -> L.elem (fst a) lowestnbrs) distvalpairs)
    where lowestnbrs = take nbr $ sort $ map fst distvalpairs
          distvalpairs = map (\x@(a,b) -> ((sqrt $ sum $ map (^2) (zipWith (-) a pnt)), b)) pntlst
            


mostcommon (x:xs)
    | count x (x:xs) == maxls (counts (x:xs)) = x
    | otherwise = mostcommon (filter (/=x) xs)
        where count x xl = (length $ filter (==x) xl)
              counts xl = foldl (\a x -> a ++ [count x xl]) [] xl

maxls (x:xs)
    | xs == [] = x
    | otherwise = max x (maxls xs)

rsqr :: [Float] -> [Float] -> (Float -> Float) -> Float
rsqr yls xls fun = 1.0 - (rss / tss)
    where rss = sum $ zipWith (\y x -> (y - (fun x))^2) yls xls
          tss = sum [(y - (xpec yls)) ^ 2 | y <- yls] 

rsqr' :: [(Float, Float)] -> (Float -> Float) -> Float
rsqr' ptls fun = 1.0 - (rss / tss)
    where rss = sum $ map (\g@(x,y) -> (y - (fun x))^2) ptls
          tss = sum [(y - yxpec) ^ 2 | y <- (map snd ptls)]
          yxpec = xpec $ map snd ptls 


linreg1var :: [(Float, Float)] -> (Float -> Float)
linreg1var ptls = (\i -> (yxpec - ) + (((sum $ map (\x y -> (x - xxpec) * (y - yxpec)) xls yls) / (sum $ map (\x -> (x - xxpec) ^2) xls)) * i))
    where xxpec = xpec xls
          yxpec = xpec yls
          xls = map fst ptls
          yls = map snd ptls

logreg :: Float -> Float -> Float -> Float
logreg int slp x = logterm / (logterm + 1.0)
    where logterm = (e ^ (int + (slp * x)))
          e = (1 + (1 / n)) ^ n
          n = 100


 