module Nayb
  (centroid
  ) where

sampledata = [(1, 1.2),(2,3.4),(2,2.1),(1,0.7),(1.5,1.3),(1.7, 1.6),(5,7),(4,3),(6,6),(10,14),(2,4),(12,8),(14,12),(5.6,6.1),(40, 36),(60, 69),(10,11),(8,9.1),(1.2,1.3),(7,7),(0.2,0.1),(19.3,20),(25,20),(30,27.2),(80,77.3),(12,10),(9,6),(35,33),(40,41),(55,49),(23,40),(11,12),(71,66),(68,73),(10,12),(40.2,38.7),(90,87.2),(100,101),(120,111),(300,290),(11,11),(33, 34),(25, 21),(47,47),(80,80.2),(99.1,97.8),(120,117.3),(30,29),(11.1,14.5)]
  
centroid :: (Num a, Fractional a) => [(a, a)] -> (a, a)
centroid x = ((sum $ map fst x) / (fromIntegral $ length x), (sum $ map snd x) / (fromIntegral $ length x)) 

dist :: (Num a, Fractional a, Floating a) => (a, a) -> (a, a) -> a
dist a b = sqrt $ (fst a - fst b) ^ 2 + (snd a - snd b) ^ 2

knn :: (Num a, Fractional a, Floating a) => a
knn = 2

calcRScore :: (Fractional a, Floating a) => [(a, a)] -> a -> a
calcRScore points slope = (sum $ map (\x -> (snd x - (slope * (fst x - cx) + cy))^2) points) 
    where 
        cx = fst $ centroid points
        cy = snd $ centroid points    


reg :: (Enum a, Ord a, Num a, Fractional a, Floating a) => [(a, a)] -> a
reg list = listmin [calcRScore list x | x <- [-1,-0.999..1]]

listmin :: (Ord a) => [a] -> a
listmin (x:[]) = x
listmin (x:xs) = min x (listmin xs)

