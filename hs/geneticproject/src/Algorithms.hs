module Algorithms (

) where

import LinearAlgebra

type Network = [(Tensor Double, Tensor Double)]

feed :: Network -> Tensor Double -> Tensor Double
feed net input = head $ foldl (\list elem@(weight, bias) -> list ++ [fmap (\a -> 1 / (1 + exp a)) $ ta (bias) (mm (last list) weight)]) [input] net

--takes networks, scores them by fitness, then takes the "topnum" and varies / crossbreeds them to create new generation
evolve :: [Network] -> [Network] -> Int -> (Network -> Double) -> [Network]
evolve nets rands topnum fitness = nets
       where eugenes = map fst $ take topnum $ quickSort (map (\i -> (i, fitness i)) nets)

--sort the nets. yay!
quickSort :: [(Network, Double)] -> [(Network, Double)]
netSortg a@(x:xs)
	  | xs == [] = [x]
	  | otherwise = quickSort (filter (\p -> snd p <= snd x) a) ++ [x] ++ quickSort (filter (\p -> snd x <= snd p) a)


