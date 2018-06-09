module Layer 
(
    Network(..),
    Layer(..),
    Weights(..),
    makeNetwork,
    calcNetwork,
    layersApply,
    weightsToVals,
    f2r
) where

import qualified Data.Sequence as S
import MatrixMaths

--Layer in a neural network
type Layer = [Double] 

--The weights have a length equal to (layer before) * (layer after)
--Each segment of length (layer before) of the weights is multiplied by their corresponding element of layer before
type Weights = [Double]

--Just a way to hold data together
data Network = Network {
    weights :: [Weights],
    layers :: [Layer],
    afuncs :: [[Double] -> Double -> Double]
}

makeNetwork :: [Int] -> Network
makeNetwork dims = Network {weights=weights, layers=layers, afuncs=(repeat (\a b -> sum a + b))}
    where len = length dims
          layers = map (\a -> take a $ repeat 1.0) dims
          weights = map (\x -> take x (repeat 1.0)) (mult dims)
          mult ls@(x:y:ys)
            | ls == [] = []
            | ys == [] = [x * y]
            | otherwise = (x * y):mult (y:ys) 

calcNetwork :: Network -> Network
calcNetwork net = Network {weights=w, layers=(layersApply w l a), afuncs=a} 
    where w = weights net
          l = layers net
          a = afuncs net

layersApply :: [Weights] -> [Layer] -> [([Double] -> Double -> Double)] -> [Layer]
layersApply weights layers activations = reverse $ foldl (\l w -> (zipWith (activations !! (length l)) (weightsToVals w (layers !! (length l))) (layers !! (succ $ length l))):l) [] weights

weightsToVals :: Weights -> Layer -> [[Double]] 
weightsToVals weights layer
    | weights == [] = []
    | otherwise = [(zipWith (*) stuff layer)] ++ (weightsToVals rest layer)
        where len = length layer 
              stuff = take len weights
              rest = drop len weights

{-
applyWeights :: Weights -> Layer -> Layer -> Layer
{-
    Takes a weights scheme, the input and output layer, and returns the output layer  
-}
applyWeights (Weights weightList) (Layer inputs) (Layer outputs) = foldl' weightToDoubleList outputs weights
    where weightToDoubleList = (\x, y@(start, end, weight) -> splitAt end x (inputs !! start) * ])
          getBefore list num = reverse $ tail $ reverse $ fst $ splitAt list num  
          getCurrentElem list num = last $ fst $ splitAt list num
          getAfter list num = snd $ splitAt list num
-}

f2r f
    | closeToFloor f = (floor f, 1)
    | closeToFloor invs = (1, floor invs)
    | closeToCeil f = (ceiling f, 1)
    | closeToCeil invs = (1, ceiling invs)
    | floor f == 0 = flippo $ incorp 0 (f2r invsTail)
    | otherwise = incorp (floor f) (f2r tail) 
        where thresh = 0.00001
              closeToFloor n = (abs $ (fromIntegral $ floor n) - n) < thresh 
              closeToCeil n = (abs $ (fromIntegral $ ceiling n) - n) < thresh
              incorp int frac@(a,b) = (int * b + a, b)
              flippo (a,b) = (b,a)
              tail = f - (fromIntegral $ floor f) 
              invs = 1.0 / f
              invsTail = 1.0 / tail

{-
makeNetwork :: [Int] -> Network
makeNetwork dims = Network weights layers (repeat (\a b -> sum a + b))
    where len = length dims
          layers = map (\a -> take a $ repeat 1.0) dims
          weights = map (\x -> take x (repeat 1.0)) (mult dims)
          mult (x:y:ys)
            | ys == [] = []
            | otherwise = (x * y):mult (y:ys) 


calcNetwork :: Network -> Network
calcNetwork (Network weights layers activs) = Network weights (layersApply weights layers activs) activs 
-}
