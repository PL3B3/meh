module Algorithms (

) where

import LinearAlgebra

--default SIGMOID feedforthwards
feed :: [[[Tensor Double]]] -> Tensor Double -> Tensor Double
feed net input = last $ foldl (\list elem@[weight:bias:[]] -> list ++ [fmap (\a -> 1 / (1 + exp a)) $ ta (bias) (mm (last list) weight)]) [input] net


--play 2048 once, return net and score
play :: [[[Tensor Double]]] -> ([[[Tensor Double]]], Int)

