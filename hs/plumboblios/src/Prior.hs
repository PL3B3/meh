module Prior
  ( Snap,
    Check,
    dts,
    dataToSnapList,
    checkToCheckMap,
    funkify,
    singleSnap,
    makeFrame,
    appendFrame,
    findFrame,
    maybeToReal,
    evalToBool,
    snapToBool,
    trueSnaps,
    portionTrueSingle,
    portionTrueMulti,
    condProb,
    bCondProb,
    intscProb,
    unionProb,
    complement
  ) where

{-
DOTHIS

handle missing data

REFERENCE

Snap = a world of variables, an array of tuples: (variablename :: string, variableval :: string)
Frame = reference term for a variable of a Snap

-}

import qualified Data.Char as Chr
import Data.List
import qualified Data.Map as M
import qualified Data.List as L





data Snap = Snap [(String, String)] deriving (Show, Eq, Read)


--This is for convenience: Basically stores a bunch of functions to be converted
data Check = Check [(String,(Int -> Bool))] [(String,(Float -> Bool))] [(String,(Bool -> Bool))] [(String,(String -> Bool))]




strTest = dataToSnapList [("int",strInt),("float",strFloat),("bool",strBool)]


rawInt = [61,26,92,41,80,24,2,81,29,62,65,66,44,49,75,73,19,92,51,84,69,74,59,84,11,62,79,73,51,62,74,76,99,32,12,92,33,47,91,74,38,66,81,27,49,67,1,88,32,50,63,54,27,60,81,10,65,85,33,38,73,83,63,61,20,69,32,29,34,13,19,12,79,98,69,60,64,36,36,78,67,89,30,77,87,55,77,5,61,92,87,40,36,99,97,15,73,62,64,95]
rawFloat = [95.546986486552,35.867062926231,82.89365599998,9.6136612862412,95.245223443604,16.848655146011,65.769846255784,66.78825820181,31.284368006179,64.17132297725,9.3903943940021,42.878438645452,24.734270211651,56.373738058085,53.199846787937,19.242719430124,99.934951355651,36.359196452591,86.16898394477,96.865480950505,43.264017367393,89.382873237777,85.174811345141,15.436126718035,13.52721788619,72.900193777355,21.290883012717,98.14338968049,19.93336371143,48.289789095656,65.691549920333,81.190617792863,16.717280920929,0.037883501517532,71.553308782891,85.197481319866,60.251576947165,19.476299322898,48.713615279977,20.835761223378,88.290482055531,9.7514613111277,50.304189953163,84.080934703388,99.082576715892,51.394629269556,48.960216785297,12.712889682834,43.245119155964,7.0003754026258,88.719069533385,2.4217278242212,68.707097586574,88.157762348725,30.051442110004,2.6639634290077,10.349127608514,91.280049500652,92.942623883925,82.597454489487,10.040956181493,19.490866139294,91.922832975128,62.660304858657,63.668657263587,56.848218178772,69.830091516408,77.180396149485,15.703114036332,82.187044938182,45.796524708065,65.337654233602,86.023187072027,50.041970540789,65.51224285062,19.196171741558,72.272842364513,84.819318347061,11.952138418309,2.8456671642352,87.941581843394,39.170986339064,55.324163546471,61.721462040079,28.446780298113,62.478841358087,54.503828778166,50.584327639353,92.521449174975,29.216556404352,0.016240077100806,34.633695443456,38.815948804289,0.36753285693356,75.833310455006,10.266890241889,9.1883094558438,78.306270986007,49.679946550019,49.894072790581]
rawBool = [False,True,False,True,False,True,False,False,True,True,True,False,False,True,True,True,False,False,False,True,True,True,True,True,False,False,True,True,False,False,True,True,True,True,True,True,False,False,True,False,True,True,False,True,True,False,False,True,True,True,True,True,False,False,True,True,True,False,True,False,False,False,True,False,True,True,True,True,True,True,True,False,True,False,False,True,True,False,False,True,False,False,True,True,False,False,False,True,True,True,True,False,False,False,True,False,False,False,True,True]


strInt = dts rawInt
strFloat = dts rawFloat
strBool = dts rawBool


checkInt1 = [("int",(\x -> mod x 3 /= 0)),("int",(>= 4))]
checkInt2 = [("int",(\x -> mod x 2 == 0 || x > 50))]
checkBul1 = [("bool",(==True))]
checkBul2 = [("bool",(==False))]
checkFlt1 = [("float", (>= 11.2))]
checkFlt2 = [("float", (\x -> x < 70.0))]


check1 = Check checkInt1 checkFlt1 checkBul1 []
check2 = Check checkInt2 checkFlt2 checkBul2 []
check3 = Check checkInt2 [] [] []
check4 = Check [] checkFlt1 checkBul1 []




stringify :: (Show a) => a -> String
stringify = show


--dataToString
dts :: (Show a, Read a) => [a] -> [String]
dts = map show


dataToSnapList :: [(String, [String])] -> [Snap]
dataToSnapList dat
  | (snd $ head dat) == [] = []
  | otherwise = (Snap (zip (map fst dat) (map head (map snd dat)))):dataToSnapList (map (\x@(a,(g:gs)) -> (a,gs)) dat)


--Takes the convenience type Check and converts all its (a -> bool) functions to (string -> bool)
checkToCheckMap :: Check -> [(String, (String -> Bool))]
checkToCheckMap (Check intcx fltcx bulcx strcx) = zippityboop intcx ++  zippityboop fltcx ++  zippityboop bulcx ++  zippityboop strcx 
  where zippityboop x = zip (map fst x) (map funkify (map snd x))


--make an 'a -> Bool' to a 'string -> Bool'
funkify :: (Show a, Read a) => (a -> Bool) -> (String -> Bool)
funkify func = (\x -> func (read x))


singleSnap :: (Show a, Read a) => String -> a -> Snap
singleSnap str dat = Snap [(str, show dat)]


makeFrame :: (Show a, Read a) => String -> a -> (String, String)
makeFrame str dat = (str, show dat)


appendFrame :: (String, String) -> Snap -> Snap
appendFrame frame (Snap g) = Snap (frame:g) 


findFrame :: Snap -> String -> Maybe (String, String)
findFrame (Snap (x:xs)) str
  | fst x == str = Just x
  | xs == [] = Nothing
  | otherwise = findFrame (Snap xs) str


maybeToReal :: Maybe a -> a
maybeToReal (Just a) = a


evalToBool :: Maybe (String, String) -> (String -> Bool) -> Maybe Bool
evalToBool frame func 
  | frame == Nothing = Nothing
  | otherwise = Just (func $ snd (maybeToReal frame))


snapToBool :: Snap -> String -> (String -> Bool) -> Maybe Bool
snapToBool snap str func = evalToBool (findFrame snap str) func


trueSnaps :: [Snap] -> [(String, (String -> Bool))] -> [Snap]
trueSnaps sL checkNet = filter (\x -> all (== Just True) [snapToBool x (fst a) (snd a) | a <- checkNet]) sL


portionTrueSingle :: (Num b, Fractional b) => [Snap] -> String -> (String -> Bool) -> b
portionTrueSingle snapList str func = (sum $ map tru snapList) / (sum $ map has snapList)
  where tru snap | snapToBool snap str func == Just True = 1.0 | otherwise = 0.0
        has snap | findFrame snap str == Nothing = 0.0 | otherwise = 1.0


portionTrueMulti :: (Num b, Fractional b) => [Snap] -> [(String, (String -> Bool))] -> b
portionTrueMulti snapList checkNet = (sum $ map tru snapList) / (sum $ map has snapList)
  where tru snap | all (== Just True) [snapToBool snap (fst a) (snd a) | a <- checkNet] = 1.0 | otherwise = 0.0
        has snap | elem Nothing [findFrame snap (fst a) | a <- checkNet] = 0.0 | otherwise = 1.0


condProb :: (Num b, Fractional b) => [Snap] -> [(String, (String -> Bool))] -> [(String, (String -> Bool))] -> b
condProb snapList evnt cond = (portionTrueMulti snapList (evnt ++ cond)) / (portionTrueMulti snapList cond)


bCondProb :: (Num b, Fractional b) => [Snap] -> [(String, (String -> Bool))] -> [(String, (String -> Bool))] -> b
bCondProb sL evt cnd = (condProb sL cnd evt) * (portionTrueMulti sL evt) / (portionTrueMulti sL cnd)


intscProb :: (Num b, Fractional b) => [Snap] -> [(String, (String -> Bool))] -> [(String, (String -> Bool))] -> b
intscProb sL evt cnd = portionTrueMulti sL (evt ++ cnd)


unionProb :: (Num b, Fractional b) => [Snap] -> [(String, (String -> Bool))] -> [(String, (String -> Bool))] -> b
unionProb sL evt cnd = (sum $ map (portionTrueMulti sL) [evt, cnd]) - (intscProb sL evt cnd)

  
complement :: Double -> Double
complement = negate . subtract 1.0



{-
satisfecho = filter (\x -> maybeToReal $ snapToBool x str func) snapList
          | findFrame (head snapList) str == Nothing = 
          |
-}

{-
old non-string functions

bCondProb :: (Show a, Read a, Num b, Fractional b) => [Snap] -> [(String, (a -> Bool))] -> [(String, (a -> Bool))] -> b
bCondProb sL evt cnd = (condProb sL cnd evt) * (portionTrueMulti sL evt) / (portionTrueMulti sL cnd)

condProb :: (Show a, Read a, Num b, Fractional b) => [Snap] -> [(String, (a -> Bool))] -> [(String, (a -> Bool))] -> b
condProb snapList evnt cond = (portionTrueMulti snapList (evnt ++ cond)) / (portionTrueMulti snapList cond)

portionTrueMulti :: (Show a, Read a, Num b, Fractional b) => [Snap] -> [(String, (a -> Bool))] -> b
portionTrueMulti snapList checkNet = (sum $ map tru snapList) / (sum $ map has snapList)
  where tru snap | all (== Just True) [snapToBool snap (fst a) (snd a) | a <- checkNet] = 1.0 | otherwise = 0.0
        has snap | elem Nothing [findFrame snap (fst a) | a <- checkNet] = 0.0 | otherwise = 1.0

portionTrueSingle :: (Show a, Read a, Num b, Fractional b) => [Snap] -> String -> (a -> Bool) -> b
portionTrueSingle snapList str func = (sum $ map tru snapList) / (sum $ map has snapList)
  where tru snap | snapToBool snap str func == Just True = 1.0 | otherwise = 0.0
        has snap | findFrame snap str == Nothing = 0.0 | otherwise = 1.0

trueSnaps :: (Show a, Read a) => [Snap] -> [(String, (a -> Bool))] -> [Snap]
trueSnaps snapList checkNet = filter (\x -> all (== Just True) [snapToBool x (fst a) (snd a) | a <- checkNet]) snapList

snapToBool :: (Show a, Read a) => Snap -> String -> (a -> Bool) -> Maybe Bool
snapToBool snap str func = evalToBool (findFrame snap str) func

evalToBool :: (Show a, Read a) => Maybe (String, String) -> (a -> Bool) -> Maybe Bool
evalToBool frame func 
  | frame == Nothing = Nothing
  | otherwise = Just (func (read $ snd $ maybeToReal frame))

-}

