module Jimbo
  ( Probability
--  , complement
--  , intersect
--  , condition
--  , binomialCDF
--  , binomialPDF
--  , geometricCDF
--  , geometricPDF
--  , marginError
  ) where

import qualified Data.Char as Chr

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Eq, Ord, Show, Read, Bounded, Enum)

next :: (Enum a, Bounded a) => a -> a
--next e = eList !! rem (fromEnum e + 1 + eLength) eLength
  --where eList = enumFrom (asTypeOf minBound e)
    --    eLength = length eList
next e
  | fromEnum e == (length (enumFrom (asTypeOf minBound e))) - 1 = toEnum 0
  | otherwise = succ e

prev :: (Enum a, Bounded a) => a -> a
prev e
  | fromEnum e == 0 = toEnum $ (length (enumFrom (asTypeOf minBound e))) - 1
  | otherwise = pred e

{-
val is the numerical value of the probability, like 50 percent chance of bla bla
evt is the value of the state of the variable in the environment that we're looking at. Like we want the probabiltiy of a coin landing on heads. The environment has a coin which has a boolean true or false aka heads or tails. Our probability data thing is a Probability Bool where the EVT is TRUE, we're looking for the TRUE value of the VAR which is the coin in the environment
var is the var we're examining: Coin for coin flip. Temperature for whatev problem with clim8 change but it's a hoax so don't sweat it
wld is the current state of the boi
-}
data Probability a = Probability {
  val :: Double,
  var :: Variable a,
  evt :: a,
  wld :: Environment
                  } deriving (Show, Eq, Read)

data PDist a = PDist [Probability a] deriving (Show, Eq, Read)

data Environment = Environment ([Variable Int], [Variable Bool], [Variable Float]) deriving (Show, Eq, Read)

data Variable a = Variable {
  nam :: Char,
  stt :: a
                  } deriving (Show, Eq, Read)
{-
complement :: Probability -> Probability
complement (Probability v e c) = Probability (1.0 - v) switch_e c
  where switch_e
          | Chr.isLower e = Chr.toUpper e
          | otherwise = Chr.toLower e

intersect :: Probability -> Probability -> Probability
intersect (Probability baseValue baseEvent baseCond) (Probability newValue newEvent newCond) = Probability (baseValue * newValue) ([baseEvent] ++ " && " ++ [newEvent]) (baseCond ++ newCond)
-}
