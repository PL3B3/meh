--Q!, 10-5-17
--Some stats functions. Very basic. Very sad
import System.IO
import Data.List
import System.Directory

xp :: Float -> Int -> Float
xp base power = product ((take power (repeat base)) ++ [1.0])

avg :: [Float] -> Float 
avg x = (sum x) / (fromIntegral  (length x))

variance :: [Float] -> Float
variance x = sum [xp (a - (avg x)) 2| a <- x]

--sqrt :: Double -> Double
--sqrt x = sqrt x (x / 2) .00001
--sqrt x guess acc
--  |done = guess
--  |otherwise = sqrt x guess2 acc
--  where
--    done = abs (guess * guess - x) < (acc * x)
--    guess2 = (guess + x / guess) / 2
    


blobSort :: Ord a => [a] -> [a]
blobSort [] = []
blobSort (x:xs) = (blobSort under) ++ [x] ++ (blobSort over)
  where
    under = filter (< x) xs
    over = filter (>= x) xs

--chebsort :: Ord a => [a] -> [a]

--grabData :: FilePath a => a -> [a]
grabData c = getDirectoryContents c
