module Architecture (

) where

import LinearAlgebra

type Layer = ((Acti, Flavor), [Tensor Double])

type Trans = ((Flavor, String), [String]) deriving Show

type Pointer = (Trans, Trans)

data Flow = Linear | Repeat deriving Show
data Acti = Tanh | Sigmoid | Relu deriving Show
data Flavor = In | Out | Feed | Recur | Convol | Split | Merge deriving Show


name :: Trans -> String
name trans@((f, n), data) = n

flavor :: Trans -> Flavor
flavor trans@((f, n), data) = f

filterFrom :: [Pointer] -> String -> [Pointer]
filterFrom pList name = filter (\x -> name == snd x) pList

filterToward :: [Pointer] -> [Pointer]
filterToward pList name = filter (\x -> name == fst x) pList

--pointerCombine :: Pointer -> Pointer -> Pointer
--pointerCombine p1@(Pointer t1@((flv1, flo1), str1) t2@((flv2, flo2), str2)) p2@(Pointer t3@((flv3, flo3), str3) t4@((flv4, flo4), str4)) = 

--feedforth the crumbulus!
crumbulator :: [Pointer] -> 


crossEntropy :: Double -> Double -> Double
crossEntropy activation target = negate $ (target * (log activation)) + ((1.0 - target) * (log $ 1.0 - activation)) 

sigmoid :: Double -> Double
sigmoid number = 1.0 / (1.0 + (exp number))

sigmoidD :: Double -> Double
sigmoidD number = negate $ (exp number) * ((1 + (exp number)) ^ (-2))

type Transform a = ((Tensor a -> Tensor a), (Tensor a -> Tensor a)) 

wa2 = (makeTensor [4,6] [1..24])
ba2 = (makeTensor [1,6] [1..6])

a = makeTensor [1,4] [1..4]
wa = [(makeTensor [4,6] [1..24]), (makeTensor [6,2] [1..12])]
bb = [(makeTensor [1,6] [1..6]), (makeTensor [1,2] [1..2])]
ab = take 2 $ repeat (\a -> log a)



--wo :: Layer a -> Tensor a
--wo l@(Layer ((a, b), c)) = a

--bo :: 

--constructFeed weights biases activation = (\a -> fmap activation $ ta (mm a (sd weights [1,0])) biases)

feed :: (Num a) => Tensor a -> Tensor a -> Tensor a -> Tensor a 
feed input weight bias = ta (mm input (sd weight [1,0])) bias

feed2 :: (Num a) => Tensor a -> Tensor a -> (a -> a) -> (Tensor a -> Tensor a)
feed2 w b a = (\i -> fmap a $ ta b $ mm (sd w [1,0]) i)

--back :: (Num a) => Tensor a -> Tensor a -> (a -> a) -> 
--back w b ad = (\g -> 

feedRelu w b = feed2 w b (\a -> maximum a 0.0)
feedSigmoid w b = feed2 w b (\a -> exp a / (exp a + 1.0)


--takes a layer as input and "applies" the layer to an input, with an output of activations
--feedControl :: Layer -> Tensor Double -> Tensor Double
--feedControl layer@((acti, flavor), datum) input = case flavor of Feed -> fmap acti $ feed input (datum !! 0) (datum !! 1) acti
                                                                 Recur -> fmap acti $ feedRecurrent datum
							         Convol -> fmap acti $ feedConvolution datum
								 

--feedConvolution

--feedRecurrent
feedRecurrent :: Layer -> Tensor Double ->  Tensor Double
feedRecurrent layer@((acti, flavor), datum) input@(Tensor shape data) =  

--RecurrentDatum = [

--back :: 

feedForward :: (Num a) => Tensor a -> [Tensor a] -> [Tensor a] -> [a -> a] -> [Tensor a]
feedForward inputs weights biases activations = foldl (\list index -> list ++ [feed (last list) (weights !! index) (biases !! index) (activations !! index)]) [inputs] [0..(pred $ length weights)]
