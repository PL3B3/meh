module FeedForward
(
    Net,
    Activation,
    fst3,
    snd3,
    thr3,
    activation,
    activationDeriv,
    calcMatrix,
    calcActivations,
    getGradients,
    getGradientNet
) where

import Layer
import MatrixMaths
import System.Random

--Neural network datatype: Array of weights and biases
data Net = Net [(Matrix Double, [Double], Activation)] deriving (Show, Eq)

data Activation = SIG | TANH deriving (Show, Eq)

tNet = Net [(tm,(take 4 $ repeat 0.2), SIG),(tm2,(take 2 $ repeat 0.2), SIG)]

tm = matrix [4,5] (take 20 $ repeat 0.05)
tm2 = matrix [2,4] (take 8 $ repeat 0.05)

fun (Net layers) = thr3 $ layers !! 0

fst3 :: (a, b, c) -> a
fst3 (g, _, _) = g

snd3 :: (a, b, c) -> b
snd3 (_, g, _) = g

thr3 :: (a, b, c) -> c
thr3 (_, _, g) = g

activation :: Activation -> (Double -> Double)
activation a = case a of SIG -> (\x -> 1.0 / (1.0 + (exp (-x))))
                         TANH -> (\x -> (2.0 / (1.0 + (exp ((-2) * x)))) - 1.0)

activationDeriv :: Activation -> (Double -> Double)
activationDeriv a = case a of SIG -> (\x -> (exp (-x)) / ((1.0 + (exp (-x))) ^ 2))
                              TANH -> (\x -> 4.0 * (exp ((-2) * x)) / ((1.0 + (exp ((-2) * x))) ^ 2))

--FeedsForward a matrix from input. Outputs a list (head:tail) where head is the final output, and tail is the z-values (w . a + b), which is basically the values without the activation, in reverse order from final layer
calcMatrix :: Net -> [Double] -> [[Double]]
calcMatrix (Net layers) inputs = (\a@(g:gs) -> g:(reverse gs)) (foldl (\b@(x:xs) y@(weights,biases,activ) -> [(map (activation activ) (calc x weights biases))] ++ xs ++ [calc x weights biases]) [inputs] layers)
  where calc a w b = zipWith (+) (matrixByVec w a) b

--FeedForward. Ordered list of activations, including final output layer
calcActivations :: Net -> [Double] -> [[Double]]
calcActivations (Net layers) ins = tail $ foldl (\b y@(weights, biases, activ) -> b ++ [map (activation activ) (calc (last b) weights biases)]) [ins] layers
  where calc a w b = zipWith (+) (matrixByVec w a) b

--List of LAYER gradients, from first layer to last layer, BUT not the ACTUAL GRADIENTS per weight and bias -- Gradients based on 1/2 (y - ypred) ^ 2
getGradients :: Net -> [Double] -> [Double] -> [[Double]]
getGradients net@(Net layers) ins targets = reverse layerGradients
  where values = calcMatrix net ins
        outs = head values
        outzs = head $ tail values
        zs = tail $ tail values
        headneck g = reverse $ tail $ reverse g
        outputGradients = foldl (\x y -> (((outs !! y) - (targets !! y)) * (activationDeriv (thr3 (layers !! y)) $ outzs !! y)):x) [] [0..(pred $ length targets)]
        layerGradients = foldl (\a b -> a ++ [hMult (matrixByVec (transpose $ fst3 $ layers !! (succ b)) (last a)) (map (activationDeriv (thr3 $ layers !! b)) (zs !! b))]) [outputGradients] (reverse $ [0..((length layers) - 2)])

getGradientNet :: Net -> [Double] -> [Double] -> Net
getGradientNet net@(Net layers) ins outs = Net (reverse nablas)
  where gradients = getGradients net ins outs
        activs = reverse $ tail $ reverse $ calcActivations net ins
        nablas = foldl (\x y -> ((matrix (dimsFromMatrix (fst3 (layers !! y))) ()), (gradients !! y), (thr3 (layers !! y))):x) [] [0..(pred $ length gradients)]
