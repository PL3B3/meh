--Q!, 10-5-17
--Some stats functions. Very basic. Very sad
import System.IO
import Data.List
import System.Directory

avg :: [Float] -> Float 
avg x = (sum x) / (fromIntegral  (length x))

blobSort :: Ord a => [a] -> [a]
blobSort [] = []
blobSort (x:xs) = (sort under) ++ [x] ++ (sort over)
  where
    under = filter (< x) xs
    over = filter (>= x) xs

--chebsort :: Ord a => [a] -> [a]

--grabData :: FilePath a => a -> [a]
grabData c = getDirectoryContents c
