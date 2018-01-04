module Posterior
  ( PDist
  ) where

import Prior as P
import Data.List as L

{-
DOTHIS

implement probability distribution

-}

data PDist = PDist [(String, (String -> Bool))] [Snap]




--calcDiscretePDist

--calcContinuousPDist

pooledProbability :: (Num a) => Double -> Double -> a -> a -> Double
pooledProbability p1 p2 n1 n2 = (p1 * n1 + p2 * n2) / (n1 + n2)

stdError :: (Num a) => Double -> Double -> a -> a -> Double
stdError p1 p2 n1 n2 = 

stdErrorPooled :: Double -> 
stdErrorPooled proportion 

sigDiffProp :: (Num a, Fractional a) => (Double-> Double -> Bool) -> (Double -> Double -> Bool) -> Double -> Double -> a -> a -> a
sigDiffProp null alt p1 p2 n1 n2 = 