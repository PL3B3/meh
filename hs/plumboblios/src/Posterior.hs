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




calcDiscretePDist

calcContinuousPDist
