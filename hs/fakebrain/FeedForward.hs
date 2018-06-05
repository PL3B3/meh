module FeedForward
(
    Training
) where

import Layer

type Training = [(Layer, Layer)] 

cost :: Network -> Training -> Double
cost net training = sum evaluated
    where newNetwork iputs = calcNetwork Network {weights=w, layers=iputs:(tail l), afuncs=a}
          w = weights net
          l = layers net
          a = afuncs net  
          compareFloatLists a b = sum $ zipWith (\x y -> (x - y) ^2) a b
          evaluated = foldl (\x y -> (compareFloatLists (last $ layers $ newNetwork (fst y)) (snd y)):x) [] training

sigmoid' a = 1.0 / (1.0 + (exp $ negate a))
--backPropagate :: Network -> Training -> Network

{-
sigmoid :: Double -> Double
sigmoid z
    | z >= (21.320903) = 1.0
    | z <= (-21.320903) = 0.0
    | otherwise = 1.0 / (1.0 + (exp $ negate z)) 
        where exp h = sum (map (\x -> (h ^ x) / (fromIntegral (fact x))) [0..150])
              fact x = product [1..x]   
-}