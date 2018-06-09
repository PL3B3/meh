module FeedForward
(
    Training,
    cost,
    backPropagate
) where

import Layer
import MatrixMaths
import System.Random

--Neural network datatype: Array of weights and biases
data Net = Net [Matrix Double] [[Double]]

type Training = [(Layer, Layer)] 

cost :: Network -> Training -> Double
cost net training = sum evaluated
    where newNetwork iputs = calcNetwork Network {weights=w, layers=iputs:(tail l), afuncs=a}
          w = weights net
          l = layers net
          a = afuncs net  
          compareFloatLists a b = sum $ zipWith (\x y -> (x - y) ^2) a b
          evaluated = foldl (\x y -> (compareFloatLists (last $ layers $ newNetwork (fst y)) (snd y)):x) [] training

--backPropagate :: Network -> Training -> Network

=======
>>>>>>> dbc4a742da1b77f9870096d7113a541be6abf6eb
