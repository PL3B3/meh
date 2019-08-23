--Q!, 10-5-17
--Some stats functions. Very basic. Very sad

module Stats (
avg,
variance,
std_dev,
median
) where

import System.IO
import Data.List
import System.Directory

xp :: (Num a, Fractional a) => a -> Int -> a
xp base power = product ((take power (repeat base)) ++ [1.0])

avg :: [Double] -> Double 
avg x = (sum x) / (fromIntegral  (length x))

variance :: [Double] -> Double
variance x = sum [xp (a - (avg x)) 2| a <- x]

std_dev :: [Double] -> Double
std_dev x = sqrt $ variance x

blobSort :: Ord a => [a] -> [a]
blobSort [] = []
blobSort (x:xs) = (blobSort under) ++ [x] ++ (blobSort over)
  where
    under = filter (< x) xs
    over = filter (>= x) xs

median :: [Double] -> Double
median [] = 0
median x
  | odd (length xSort) = xSort !! midDex
  | even (length xSort) = avg [xSort !! midDex, xSort !! (midDex - 1)]
  where xSort = blobSort x
        midDex = floor ((fromIntegral (length xSort)) / 2)

skew :: [Double] -> [Char]
skew [] = "No skew - It is blank, and you are stupid for asking in the first place. It's not like I also have a life I need to live, and I could actually be earning money to feed my wife and kids right now. oooohh nooo. Mr. Customer comes first with his stupid fucking inquiries that a three year old could answer"
skew x
  | avg x > median x = "RIGHT"
  | avg x < median x = "LEFT"
  | avg x == median x = "I believe center"

--chebsort :: Ord a => [a] -> [a]

--grabData :: FilePath a => a -> [a]
grabData c = getDirectoryContents c


--sqrt' x = sqrt' x (x / 2) 0.001
sqrt' x g 
  | abs (x - (g * g)) <= 0.001 = g
  | otherwise = sqrt' x (g + ((x - (g * g)) / x))

--some calculus functions

--rpn calculator
rpn :: (Fractional a, Num a, RealFrac a, Read a) => String -> a -> String -> a
rpn p v str = head (foldl fun [] (words str))
  where fun (x:y:ys) "+" = (x + y):ys
        fun (x:y:ys) "-" = (x - y):ys
        fun (x:y:ys) "*" = (x * y):ys
        fun (x:y:ys) "/" = (x / y):ys
        fun (x:y:ys) "^" = (xp x (floor y)):ys
        fun (x:xs) "sqrt" = sqrt' x (x / 2):xs
        fun xs num
          | num == p = v:xs
          | otherwise = read num:xs

rpn' :: (Fractional a, Num a, RealFrac a, Read a) => String -> a
rpn' = head . foldl fun [] . words
  where fun (x:y:ys) "+" = (x + y):ys
        fun (x:y:ys) "-" = (x - y):ys
        fun (x:y:ys) "*" = (x * y):ys
        fun (x:y:ys) "/" = (x / y):ys
        fun (x:y:ys) "^" = (xp x (floor y)):ys
        fun xs num = read num:xs

lim p n xpr = [(rpn p (n + 0.000000001) xpr), (rpn p (n - 0.000000001) xpr)] 
lim :: (Fractional a, Num a, RealFrac a, Read a) => String -> a -> String -> [a]

deriv :: (Fractional a, Num a, RealFrac a, Read a) => String -> a -> String -> a
--deriv p n xpr = [((rpn p (n + 0.0000000000001) xpr - rpn p n xpr) / 0.0000000000001), ((rpn p n xpr - rpn p (n - 0.0000000000001) xpr) / 0.0000000000001)] 

deriv p n xpr = (rpn p (n + 0.000000001) xpr - rpn p (n - 0.000000001) xpr) / 0.000000002 

composer :: [Char] -> [Char] -> String -> String
composer x char y = repl x char (words y)

repl :: [Char] -> [Char] -> [String] -> String
repl a b (c:cs)
  | cs == [] = c
  | b == c = a ++ " " ++ repl a b cs
  | otherwise = c ++ " " ++ repl a b cs
