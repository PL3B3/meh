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
          compareFloatLists a b = sum $ zipWith (\x y -> 0.5 * (x - y) ^2) a b
          evaluated = foldl (\x y -> (compareFloatLists (last $ layers $ newNetwork (fst y)) (snd y)):x) [] training

fst3 :: (a, a, a) -> a
fst3 (g, _, _) = g

snd3 :: (a, a, a) -> a
snd3 (_, g, _) = g

thr3 :: (a, a, a) -> a
thr3 (_, _, g) = g

activation :: Activation -> (Double -> Double)
activation a = case a of
  | SIG -> (\x -> 1.0 / (1.0 + (exp (-x))))
  | TANH -> (\x -> (2.0 / (1.0 + (exp ((-2) * x)))) - 1.0)

activationDeriv :: Activation -> (Double -> Double)
activationDeriv a = case a of
  | SIG -> (\x -> (exp (-x)) / ((1.0 + (exp (-x))) ^ 2))
  | TANH -> (\x -> 4.0 * (exp ((-2) * x)) / ((1.0 + (exp ((-2) * x))) ^ 2))
  

--FeedsForward a matrix from input. Outputs a list (head:tail) where head is the final output, and tail is the z-values (w . a + b), which is basically the values without the activation, in reverse order from final layer
calcMatrix :: Net -> [Double] -> [[Double]]
calcMatrix (Net layers) inputs = (\a@(g:gs) -> g:(reverse gs)) foldl (\x@(x:xs) y@(weights,biases,activ) -> (activation activ $ calc x weights biases) ++ xs ++ [calc x weights biases]) [inputs] layers
  where calc a w b = zipWith (+) (matrixByVec w a) b

--List of LAYER gradients, from first layer to last layer, BUT not the ACTUAL GRADIENTS per weight and bias
getGradients :: Net -> [Double] -> [Double] -> [[Double]]
getGradients (Net layers) ins targets = reverse $ outputGradients: 
  where values = calcMatrix net ins
        outs = head values
        outzs = head $ tail values
        zs = tail $ tail values
        outputGradients = foldl (\x y -> (((outs !! y) - (targets !! y)) * (activationDeriv (thr3 $ layers !! y) $ outzs !! y)):x) [] [0..(pred $ length targets)]
        layerGradients = 

