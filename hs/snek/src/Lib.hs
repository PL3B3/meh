module Lib
    ( someFunc
    ) where

data NestedList a = Elem a | List [NestedList a]
    
someFunc :: IO ()
someFunc = putStrLn "someFunc"

parse :: [Float] -> Float
parse g = (\(x:xs) -> x) g

myLast (x:[]) = x
myLast (x:xs) = myLast xs

myButLast (x:y:[]) = x
myButLast(x:y:ys) = myButLast (y:ys)

elemAt :: [a] -> Int -> a
--Takes the i'th element of list x
elemAt x i = last (take (succ i) x)

myLength :: (Eq a) => [a] -> Int
myLength (x:xs)
    | xs == [] = 1
    | otherwise = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse (x:[]) = [] ++ [x]
myReverse (x:xs) = (myReverse xs) ++ [x]

isPalindrome g = take halfway g == take halfway (reverse g)
    where
        halfway = (floor $ fromIntegral $ length g) 
        
isPalindrome' g = g == reverse g

flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List []) = []

compress :: (Eq a) => [a] -> [a]
compress (x:[]) = [x]
compress (x:xs)
    | x == head xs = compress ([x] ++ tail xs) 
    | otherwise = [x] ++ compress xs

excludeWhile :: (a -> Bool) -> [a] -> [a]
excludeWhile _ [] = []
excludeWhile f (x:xs)
    | f x == True = excludeWhile f xs
    | f x == False = x:xs

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x : takeWhile (==x) xs) : pack (dropWhile (==x) xs)

numCompress :: (Eq a) => [a] -> [(Int, a)]
numCompress [] = []
numCompress l = zip (map length $ pack l) (map head $ pack l)

repeatElems :: [a] -> Int -> [a]
repeatElems (x:[]) r = []
repeatElems (x:xs) r = replicate r x ++ repeatElems xs r

dropNth :: [a] -> Int -> [a]
dropNth x n
    | n < 2 = []
    | otherwise = map (x !!) (takeWhile (< (length x)) $ [g - 1| g <- [1..], mod g n > 0])

dropN :: [a] -> Int -> [a]
dropN x n
    | n == 0 = x
    | otherwise = map (x !!) (takeWhile (< (length x)) $ [g - 1| g <- [1..], g /= n])
    
split :: [a] -> Int -> ([a],[a])
split [] _ = ([],[])
split (x:xs) l
    | l == 1 = ([x], xs)
    | otherwise = (x : fst (split xs (l - 1)), snd (split xs (l - 1)))

slice :: [a] -> Int -> Int -> [a]
slice l a z = drop (a - 1) (reverse (drop (length l - z) (reverse l)))

rotate x d
    | d < 0 = (snd $ split x (length x + d)) ++ (fst $ split x (length x + d))
    | d > 0 = (snd $ split x d) ++ (fst $ split x d)