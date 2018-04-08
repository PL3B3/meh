module Grub () where

import Data.List
import Data.Maybe
import Data.Char
import Lang as L

main = do
  si <- getContents
  putStr $ func si
    where func = map toUpper

costFunc :: [L.Tape a] -> (L.Tape a -> L.Tape a) -> (L.Tape a -> L.Tape a -> Int) -> ([Int] -> Float) -> Float
costFunc testList funcToTest errorFunc cumeFunc = cumeFunc $ zipWith errorFunc testList (map funcToTest testList)

