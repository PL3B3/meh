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

evalMetaFuck :: String -> String -> String
evalMetaFuck metaStr bFuckStr = foldl (\x y -> run y x) '0':bFuckStr metaStr
    where run '+' str 
            | str !! dex == ',' = str
            | otherwise = (fst $ splitAt dex str) ++ (succc (str !! dex) "+-<>[].,") ++ (snd $ splitAt dex str)
                where dex = succ $ read (head str) :: Int
                      succc elem list = head $ tail $ dropWhile (/=elem) list  
          run '-' str
            | str !! dex == '+' = str
            | otherwise = (fst $ splitAt dex str) ++ (antisuccc (str !! dex) "+-<>[].,") ++ (snd $ splitAt dex str)
                where dex = succ $ read (head str) :: Int
                      antisuccc elem list = last $ takeWhile (/=elem) list
          run '<' str = (succ (read (head str) :: Int)):str
          run '>' str = (pred (read (head str) :: Int)):str
          run '[' str = 
          run ']' str =
          run '.' str =
          run ',' str =  

seekStringLoopL
seekStringLoopR