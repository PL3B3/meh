module Layer 
(
    Node(..),
    Layer,
    Weights,
) where

data Node = Node
{-
Type for a single node , or neuron , of the network
-}
{
    activation :: ([Float] -> Float),
    value :: Float,
    threshold :: Float
}

data Layer = Layer [Node]
{-
    Type for a layer of neurons
-}

data Weights = Weights [(Int, Int, Float)]
{-
    Type for the weights between two layers. 
    It is a list of tuples consisting of (startnodeindex) (endnodeindex) and (weight)
-}

applyWeights :: Weights -> Layer -> Layer -> Layer
{-
    Takes a weights scheme, the input and output layer, and returns the output layer  
-}
applyWeights (Weights weightList) (Layer inputs) (Layer outputs) = foldl' weightToFloatList outputs weights
    where weightToFloatList = (\x, y@(start, end, weight) -> splitAt end x (inputs !! start) * ])
          getBefore list num = reverse $ tail $ reverse $ fst $ splitAt list num  
          getCurrentElem list num = last $ fst $ splitAt list num
          getAfter list num = snd $ splitAt list num

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