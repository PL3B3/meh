module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"


{-
data Elist = Elist [Environment] deriving (Show, Eq, Read)

data Environment = Environment [Variable Int] [Variable Bool] [Variable Float] deriving (Show, Eq, Read)

data Variable a = Variable {
  nam :: String,
  val :: a
                  } deriving (Show, Eq, Read)

testList = Elist [Environment [Variable {nam = n, val = v}] [Variable {nam = m, val = b}] [] | n <- ["jebVotes"], v <- [400], m <- ["jebWon"], b <- [True]]

numGood :: (Num a, Fractional a) => Elist -> (Environment -> Bool) -> (Environment -> Bool) -> (a, a, a)
numGood (Elist g) f c = (good, total, good / total)
  where good = fromIntegral (length $ filter (\x -> (f x) && (c x)) g)
        total = fromIntegral (length $ filter c g)

pDouble :: (Num a, Fractional a) => Elist -> (Environment -> Bool) -> a
pDouble (Elist a) f = numGood (Elist a) f / (fromIntegral (length a))

complement :: Probability -> Probability
complement (Probability v e c) = Probability (1.0 - v) switch_e c
  where switch_e
          | Chr.isLower e = Chr.toUpper e
          | otherwise = Chr.toLower e

intersect :: Probability -> Probability -> Probability
intersect (Probability baseValue baseEvent baseCond) (Probability newValue newEvent newCond) = Probability (baseValue * newValue) ([baseEvent] ++ " && " ++ [newEvent]) (baseCond ++ newCond)

-}
