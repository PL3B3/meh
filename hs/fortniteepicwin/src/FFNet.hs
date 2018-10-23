module FFNet
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
    quadCost,
    crossEntropyCost,
    crossEntropyCostLs,
    makeNet,
    crossGradients,
    crossGradientNet,
    crossProp,
    crossLs,
    cross,
    absoluteUnit,    
    sumNets,
    addTwoNets,
    divideNetByScalar,
    averageNets,
    cutoffOptimal
) where

import MatrixMaths
import System.Random

repeatList ls num
  | num == 0 = []
  | otherwise = ls ++ (repeatList ls (pred num)) 

--Neural network datatype: Array of weights and biases
data Net = Net [(Matrix Double, [Double], Activation)] deriving (Show, Eq)

data Activation = SIG | TANH deriving (Show, Eq)

type Pairlist = [([Double],[Double])]

tNet = Net [(tm,(take 4 $ repeat 0.2), SIG),(tm2,(take 2 $ repeat 0.2), SIG)]

tm = matrix [4,5] (take 20 $ repeat 0.2)
tm2 = matrix [2,4] (take 8 $ repeat 0.2)

fun (Net layers) = thr3 $ layers !! 0

makeNet :: Double -> [Int] -> Activation -> Net
makeNet num dims a = Net (take (pred $ length dims) $ (foldl (\list dim -> list ++ [((matrix [(dims !! (dim + 1)), (dims !! dim)] (take (product [(dims !! dim), (dims !! (dim + 1))]) $ repeat num)), (take (dims !! (dim + 1)) $ repeat num), a)]) [] [0..(pred $ pred $ length dims)])) 

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
activationDeriv a = case a of SIG -> (\x -> if (x < (-500)) then 0.0 else (exp (-x)) / ((1.0 + (exp (-x))) ^ 2))
                              TANH -> (\x -> 4.0 * (exp ((-2) * x)) / ((1.0 + (exp ((-2) * x))) ^ 2))

quadCost :: Net -> [Double] -> [Double] -> Double
quadCost net ins tgt = sum $ zipWith (\x y -> 0.5 * (x - y) ^ 2) (head $ calcMatrix net ins) tgt

crossEntropyCost :: Net -> [Double] -> [Double] -> Double
crossEntropyCost net ins tgt = (-1.0) * (1.0 / (fromIntegral $ length tgt)) * logsum
  where logsum = sum $ zipWith (\x y -> (y * (log x)) + ((1.0 - y) * (log (1.0 - x)))) outs tgt
        outs = head $ calcMatrix net ins 

crossEntropyCostLs :: Net -> Pairlist -> Double
crossEntropyCostLs net pairs = sum $ zipWith (\x y -> crossEntropyCost net x y) (map fst pairs) (map snd pairs)

--FeedsForward a matrix from input. Outputs a list (head:tail) where head is the final output, and tail is the z-values (w . a + b), which is basically the values without the activation, in reverse order from final layer
calcMatrix :: Net -> [Double] -> [[Double]]
calcMatrix (Net layers) inputs = (\a@(g:gs) -> g:(reverse gs)) (foldl (\b@(x:xs) y@(weights,biases,activ) -> [(map (activation activ) (calc x weights biases))] ++ xs ++ [calc x weights biases]) [inputs] layers)
  where calc a w b = zipWith (+) (matrixByVec w a) b

--Same as calcMatrix except softmax activs applied to the final layer
calcMatrixSoftMax :: Net -> [Double] -> [[Double]]
calcMatrixSoftMax net@(Net layers) ins = (\a@(x:y:ys) -> [(map (\b -> ((exp b) / (sum $ map exp y))) y)] ++ y:ys)$ calcMatrix net ins

--FeedForward. Ordered list of activations, including final output layer
calcActivations :: Net -> [Double] -> [[Double]]
calcActivations (Net layers) ins = tail $ foldl (\b y@(weights, biases, activ) -> b ++ [map (activation activ) (calc (last b) weights biases)]) [ins] layers
  where calc a w b = zipWith (+) (matrixByVec w a) b

crossGradients :: Net -> [Double] -> [Double] -> [[Double]]
crossGradients net@(Net layers) ins tgt = grads
  where vals@(out:outz:zs) = calcMatrix net ins
        finals = foldl (\x y -> x ++ [(((out !! y) - (tgt !! y)) / ((out !! y) * (1.0 - (out !! y)))) * (activationDeriv (thr3 $ layers !! y) $ outz !! y)]) [] [0..(pred $ length out)]
        grads = foldl (\a b -> (hMult (matrixByVec (transpose $ fst3 $ layers !! (succ b)) (head a)) (map (activationDeriv (thr3 $ layers !! b)) (zs !! b)):a)) [finals] (reverse $ [0..((length layers) - 2)])


crossGradientNet :: Net -> [Double] -> [Double] -> Net
crossGradientNet net@(Net layers) ins outs = Net (reverse nablas)
  where gradients = crossGradients net ins outs
        activs = ins:(reverse $ tail $ reverse $ calcActivations net ins)
        nablas = foldl (\x y -> ((matrix (dimsFromMatrix (fst3 (layers !! y))) (foldl (\h j -> h ++ (map (\a -> a * j) (gradients !! y))) [] (activs !! y))), (gradients !! y), (thr3 (layers !! y))):x) [] [0..(pred $ length gradients)]

testSamps = [([1.0],[0.0]),([0.0],[1.0])]

crossProp n i o r e
  | e == 0 = n
  | otherwise = crossProp (addTwoNets n (divideNetByScalar (crossGradientNet n i o) (-(r)))) i o r (pred e)

--I am presented with a net, also a sample of data, and I gradient DESCANT to produce the final product
cross net pairlist r8 = addTwoNets net (divideNetByScalar (averageNets (map (\a@(i,o) -> crossGradientNet net i o) pairlist)) (-r8))

--I do cross but many list
crossLs net pairlist r8 gens num
  | gens == 0 = net
  | otherwise = crossLs (cross net (take num pairlist) r8) (drop num pairlist) r8 (pred gens) num

--Generates a absoluteUnit, who predic data with HIGH accurate
absoluteUnit :: Int -> Net -> Pairlist -> Double -> Int -> Int -> IO ([Double])
absoluteUnit genr8r net samps rate gens num = do
  let dexes = take (num * gens) $ randomRs (0,(pred $ length samps)) (mkStdGen genr8r)
      boi = crossLs net (foldl (\a b -> a ++ [samps !! b]) [] dexes) rate gens num
  putStrLn $ show boi
  return (zipWith (\x y -> quadCost boi x y) (map fst samps) (map snd samps))  

--Sums a bunch of nets
sumNets :: [Net] -> Net
sumNets ls@(x:y:ys)
  | ys == [] || y:ys == [] = x
  | otherwise = sumNets ((addTwoNets x y):ys)

--Sums ONLY TWO nets
addTwoNets :: Net -> Net -> Net
addTwoNets net1@(Net layers1) net2@(Net layers2) = Net addedStuff
  where addLayers l1@(w1, b1, a1) l2@(w2, b2, a2) = (addMatrix w1 w2,zipWith (+) b1 b2, a1)
        addedStuff = foldl (\x y@(bebo, bobe) -> x ++ [addLayers bebo bobe]) [] (zip layers1 layers2)

--It's all in the name
divideNetByScalar :: Net -> Double -> Net
divideNetByScalar (Net layers) num = Net (map (\l@(w, b, a) -> (scalarMult w num, map (\x -> x / num) b, a)) layers)

--Sum divided by Length
averageNets :: [Net] -> Net
averageNets nets = divideNetByScalar (sumNets nets) (fromIntegral $ length nets) 

--If the neural network isn't improving much more, there's only a risk it's overfitting, so training should stop
cutoffOptimal :: Net -> Pairlist -> Pairlist -> [Double] -> Double -> Int -> Int -> Net
cutoffOptimal net pairlist validation costTrends rate epochs batchsize
  | epochs == 0 || flatTrend costTrends = net
  | otherwise = cutoffOptimal (cross net (take batchsize pairlist) rate) (drop batchsize pairlist) validation (costTrends ++ [crossEntropyCostLs net validation]) rate (pred epochs) batchsize
    where flatTrend dataList
            | length dataList < 5 = False
            | otherwise = ((dataList !! ((length dataList) - 5)) - (last dataList)) / 5.0 < 0.1   
