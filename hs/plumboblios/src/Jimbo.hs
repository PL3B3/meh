module Jimbo
  ( --Probability
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
import Data.List
import qualified Data.Map as M
import qualified Data.List as L

data Snap = Snap [(String, String)] deriving (Show, Eq, Read)
--data Frame = 
testSnap = Snap [("int","4"), ("float","3.7"),("string","\"heyo\""),("intlist","[2,3,4]")]

--data Checknet = [(String, (a -> Bool))]

stringify :: (Show a) => a -> String
stringify = show

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

evalToBool :: (Show a, Read a) => Maybe (String, String) -> (a -> Bool) -> Maybe Bool
evalToBool frame func 
  | frame == Nothing = Nothing
  | otherwise = Just (func (read $ snd $ maybeToReal frame))

snapToBool :: (Show a, Read a) => Snap -> String -> (a -> Bool) -> Maybe Bool
snapToBool snap str func = evalToBool (findFrame snap str) func

portionTrue :: (Show a, Read a) => [Snap] -> String -> (a -> Bool) -> Float
portionTrue snapList str func = (sum $ map tru snapList) / (sum $ map has snapList)
  where tru snap | snapToBool snap str func == Just True = 1.0 | otherwise = 0.0
        has snap | findFrame snap str == Nothing = 0.0 | otherwise = 1.0


{-
satisfecho = filter (\x -> maybeToReal $ snapToBool x str func) snapList
          | findFrame (head snapList) str == Nothing = 
          |
-}
