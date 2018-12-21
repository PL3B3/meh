module Train (
             ) where

--take you down to de pain train station in train town (sobbing)

import LinearAlgebra
import Game
import System.Random

--type Network = [(Tensor Double, Tensor Double)]
type Layer = [[Tensor Double]]

--default SIGMOID feedforthwards
feed :: [[[Tensor Double]]] -> Tensor Double -> Tensor Double
feed net input = last $ foldl (\list elem@[weight:bias:[]] -> list ++ [fmap (\a -> 1 / (1 + exp a)) $ ta (bias) (mm (last list) weight)]) [input] net

zerotensor shape = makeTensor shape (take (product shape) $ repeat 0)
numtensor num shape = makeTensor shape (take (product shape) $ repeat num)

--previous hidden states + latest output
--order of layer is [weights from in to hidden, weights from hidden to next hidden, weights from hidden to out, hidden bias, out bias]
feedrecur :: [[Tensor Double]] -> [Tensor Double] -> (Double -> Double) -> ([Tensor Double], [Tensor Double])
feedrecur layers input transform = (foldl (\outs hidden -> outs ++ [(ta (dex2 layers (length outs) 4) (mm (hidden) (dex2 layers (length outs) 2)))]) [] hiddens, hiddens)
  where hiddens = tail $ foldl (\ls i -> ls ++ [fmap transform $ ta (dex2 layers i 3) (ta (mm (input !! i) (dex2 layers i 0)) (mm (last ls) (dex2 layers i 1)))]) [zerotensor (shapeOf ((layers !! 0) !! 3))] (ps (length input))
        dex2 l i i2 = (l !! i) !! i2

--takes networks, scores them by fitness, then takes the "topnum" and varies / crossbreeds them to create new generation
--evolve :: [Network] -> [Network] -> Int -> (Network -> Double) -> [Network]
evolve :: [[[[Tensor Double]]]] -> [[[[Tensor Double]]]] -> Int -> ([[[Tensor Double]]] -> Int) -> [[[[Tensor Double]]]]
evolve nets rands topnum fitness = (zipWith addNets eugenes rands)
-- ++ [netMap (\a -> a / (fromIntegral topnum)) (foldl (addNets) (head eugenes) (tail eugenes))]
-- ++ [foldl (\a b -> a ++ [(eugenes !! b) !! b]) [] [0..(pred netsize)]]
       where eugenes = map fst $ take topnum $ quickSort (map (\i -> (i, fitness i)) nets)
             netsize = length $ nets !! 0
webbi :: [[[Tensor Double]]]
webbi = [[[numtensor 4.1 [30, 10], numtensor 4.1 [1, 10]]], [[numtensor 4.1 [10, 5], numtensor 4.1 [1, 5]]]]
zebbi :: [[[Tensor Double]]]
zebbi = [[[numtensor 1.2 [30, 10], numtensor 1.2 [1, 10]]], [[numtensor 1.2 [10, 5], numtensor 1.2 [1, 5]]]]

addNets :: [[[Tensor Double]]] -> [[[Tensor Double]]] -> [[[Tensor Double]]]
addNets n1 n2 = zipWith (zipWith (zipWith (ta))) n1 n2

zipNets :: (a -> b -> c) -> [[[a]]] -> [[[b]]] -> [[[c]]]
zipNets f n1 n2 = zipWith (zipWith (zipWith (f))) n1 n2

appendNets :: [[[Tensor Double]]] -> [[[Tensor Double]]] -> [[[Tensor Double]]]
appendNets n1 n2 = zipWith (zipWith (++)) n1 n2

appendNetsUp :: [[[Tensor Double]]] -> [[[Tensor Double]]] -> [[[Tensor Double]]]
appendNetsUp n1 n2 = zipWith (++) n1 n2

netMap :: (Functor f) => (a -> b) -> [[[f a]]] -> [[[f b]]]
netMap fun net = map (map (map (fmap fun))) net

getDims :: [[[Tensor Double]]] -> [[[[Int]]]]
getDims tensors = map (map (map shapeOf)) tensors

totaldim :: [[[[Int]]]] -> Int
totaldim dims = sum $ concat $ concat $ concat (netMap product [dims])

tdimup :: [[[[Int]]]] -> [Int]
tdimup dims = concat $ concat $ concat (netMap product [dims])


--takes dims, a list of nums, and makes a net of doubles shaped like dims
sigNetMake :: [[[[Int]]]] -> [Double] -> [[[[Double]]]]
sigNetMake dims nums = tail $ foldl (\a b -> [[[[fromIntegral $ (newhead a) + (newprod b)]]]] ++ (tail a) ++ [sigLayerMake b (take (newprod b) (drop (newhead a) nums))]) [[[[0.0]]]] dims
  where newprod a = sum $ concat $ map (map product) a
        newhead = floor . head . head . head . head

sigLayerMake :: [[[Int]]] -> [Double] -> [[[Double]]]
sigLayerMake dims nums = tail $ foldl (\a b -> [[[fromIntegral $ (newhead a) + (newprod b)]]] ++ (tail a) ++ [sigStepMake b (take (newprod b) (drop (newhead a) nums))]) [[[0.0]]] dims
  where newprod a = sum $ map product a
        newhead = floor . head . head . head

sigStepMake :: [[Int]] -> [Double] -> [[Double]]
sigStepMake dims nums = tail $ foldl (\a b -> [[fromIntegral $ (newhead a) + product b]] ++ (tail a) ++ [take (product b) (drop (newhead a) nums)]) [[0.0]] dims
  where newhead = floor . head . head



exampleLayer = [[[12, 7], [1,7]], [[12, 7], [1, 7]]] :: [[[Int]]]

exampleStep = [[12, 7], [1,7]] :: [[Int]]

{-
victory_royale :: [[[[[Int]]]]] -> IO ()
victory_royale dims = do
  gen <- newStdGen
  let rands1 = randomRs (-3.0, 3.0) gen
--  rands2 <- netMap (\a -> makeTensor dims (take (product dims) $ randomRs (-0.1, 0.1) gen :: [Double])) dims
  let g = game (map netToPlayer (foldl (\l c -> l ++ [zipNets makeTensor (dims !! (length l)) (sigNetMake (dims !! (length l)) c)]) [] (fragment chunks rands1))) (zerotensor [(length dims) * (pred $ length dims), 1]) 10
  putStrLn $ show $ snd g
--  putStrLn $ show $ map snd $ fst g
    where chunks = map totaldim dims
-}

generateNet :: [[[[Int]]]] -> [Double] -> [[[Tensor Double]]]
generateNet dims num = head $ netMap (\dim -> makeTensor dim (take (product dim) num)) [dims]



--column to row
--mismatch b/w tensor and which player is referenced (not include self -> diff nums)

fragment :: [Int] -> [a] -> [[a]]
fragment seglengths list
  | seglengths == [] = [[]]
  | otherwise = [take (head seglengths) list] ++ fragment (tail seglengths) (drop (head seglengths) list)

exampledim :: [[[[Int]]]]
exampledim = [[[[12,7], [1, 7]]], [[[7,3], [1,3]]]]

xdims num = take num $ repeat exampledim

rands = [1.6920003200910525,0.5470298797797577,-1.8207187463906271,-1.163126408341688,-2.641995224140983,2.812291857255703,-1.7530527907723483,-2.801312741889189,-1.7727076271075335,1.960560595090735,1.2287856676193023,-2.8651724683786735,2.1709278722302088,0.7131334393388173,1.3262801887218671,-1.2672411778293466,1.0501502703104393,2.631662013831498,1.1832195925434323,1.2696261259212278,1.2544147388944058,2.283316737646146e-3,2.8693055786739254,-1.2241312065862908,2.543231092802814,-2.7432508850508297,0.25502533809921335,0.1505207214736184,-1.8864787930933247,-0.4698753479875437,-2.02951878901698,-0.3069188741047961,0.44458379470123344,-1.415737565429405,0.6678905788781311,-1.059595846073533,-1.8236806265987198,-1.7185729320551597,0.10417244466767928,1.0828818117184955,2.798669044344207,-2.4653034967166945,-2.5971964431159913,2.3644921167139685,0.6449422085115901,-1.4965025196702701,-2.411683791624995,-2.10366158597494,2.7368784602292866,-2.551626084943328,-1.029458552510937,-0.5698449093224083,0.9931869106478284,1.1927599615593252,0.4741663140697492,-0.6340225384107923,-2.2369603194188956,-0.3395280972654966,-1.6168315752055207,2.5481500914791395,0.989636008579315,2.9322506815601823,-1.614001203165486,-2.1730265902142856,2.628764172079121,-1.7368879845355338,3.187474295335946e-3,-0.988174190961451,-1.8762178118889337,-0.9988407328414137,0.21060998947337772,0.15809070519733837,-2.2653461766005463,2.899097523697189,-1.5893995586282916,-1.348947492424529,-0.8469149153085693,0.8481301272701289,1.089824775109916,-2.0168878835067074,2.5016680846716373,-1.9045970771330176,0.7473093295937812,-1.645339914246749,1.777133288693145,-2.267331944678597,-0.9380257544534212,2.0685447631551437,1.8991823255660059,-0.7135849032970265,-1.2853529213328876,2.3287253951180356,2.0129764840882025,-2.7526379311814333,-2.6239611770469504,-2.7235344735046656,2.92351371778852,2.1020694730266065,-2.6774172785688752,0.2723420516375974,1.0250449828122665,0.4710095753228747,-1.9594436715447492,0.27011391381510297,-5.6122436025313416e-2,1.544450723843613,1.9941023247036913,2.0483250306351852,1.4523421955526628,-1.9870090069649828,-0.5955729064365327,-5.456915691957409e-2,0.13142451779468445,-1.8170622332175757,0.38995906353567866]


exampleplayer = netToPlayer $ generateNet exampledim rands

exampleboard :: Tensor Double
exampleboard = numtensor 0.0 [12,1]

io :: IO [Int]
io = do
  gen <- newStdGen
  return $ take 10 $ (randomRs (-3, 3) gen)

netToPlayer :: [[[Tensor Double]]] -> ((Tensor Double -> [Double]), Int)
netToPlayer wbs = ((\ins -> infoOf $ feed wbs ins), 0)

--sort the nets. yay!
quickSort :: [([[[Tensor Double]]], Int)] -> [([[[Tensor Double]]], Int)]
quickSort a
  | a == [] = []
  | xs == [] = [x]
  | otherwise = quickSort (filter (\p -> snd p < snd x) a) ++ (filter (\p -> snd p == snd x) a) ++ quickSort (filter (\p -> snd x < snd p) a)
    where x = head a
          xs = tail a

