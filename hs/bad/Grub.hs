module Grub () where

import Data.List
import Data.Maybe

main = do
  si <- getContents
  putStr $ func si
    where func = map toUpper

costFunc :: [String] -> (String -> String) -> (String -> String -> Int) -> ([Int] -> Float) -> Float
costFunc testList funcToTest errorFunc cumeFunc = cumeFunc $ zipWith errorFunc testList (map funcToTest testList)

