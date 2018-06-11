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
data Net = Net [(Matrix Double, [Double], Activation)] deriving (Show, Eq)

data Activation = SIG | TANH deriving (Show, Eq)

type Training = [(Layer, Layer)] 

cost :: Network -> Training -> Double
cost net training = sum evaluated
    where newNetwork iputs = calcNetwork Network {weights=w, layers=iputs:(tail l), afuncs=a}
          w = weights net
          l = layers net
          a = afuncs net  
          compareFloatLists a b = sum $ zipWith (\x y -> (x - y) ^2) a b
          evaluated = foldl (\x y -> (compareFloatLists (last $ layers $ newNetwork (fst y)) (snd y)):x) [] training

activation :: Activation -> (Double -> Double)
activation a = case a of
  | SIG -> (\x -> 1.0 / (1.0 + (exp (-x))))
  | TANH -> (\x -> (2.0 / (1.0 + (exp ((-2) * x)))) - 1.0)

activationDeriv :: Activation -> (Double -> Double)
activationDeriv a = case a of
  

calcMatrix :: Net -> [Double] -> [[Double]]
calcMatrix (Net layers) inputs = foldl (\x@(x:xs) y@(weights,biases,activ) -> (activation activ $ calc x weights biases) ++ xs ++ [calc x weights biases]) [inputs] layers
  where calc a w b = zipWith (+) (matrixByVec w a) b



--backPropagate :: Network -> Training -> Network
