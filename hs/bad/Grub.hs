module Grub () where

import Data.List
import Data.Maybe
import Data.Char
import Lang as L

funList = []

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
                where dex = index str
                      succc elem list = head $ tail $ dropWhile (/=elem) list  
          run '-' str
            | str !! dex == '+' = str
            | otherwise = (fst $ splitAt dex str) ++ (antisuccc (str !! dex) "+-<>[].,") ++ (snd $ splitAt dex str)
                where dex = index str
                      antisuccc elem list = last $ takeWhile (/=elem) list
          run '>' str
            | index str == (pred $ length str) = str
            | otherwise = (succ (read (head str) :: Int)):(tail str)
          run '<' str
            | index str == 0 = str
            | otherwise = (pred (read (head str) :: Int)):(tail str)
          run '[' str
            | str !! dex == '+' = seekStringLoopR 0  
            | 
                where dex = index str
          run ']' str
            |
            |
          run '.' str =
          run ',' str =  
          run num str = evalMetaFuck (funList !! (fullNum num str))
            where fullNum num str = read (num:(takeWhile (\x -> elem x "0123456789") str) :: Int  
          seekStringLoopL
          seekStringLoopR
          index str = succ $ (read (head str) :: Int)
          
evalMetaFuck mStr bStr = eMF' mStr 0 bStr 0

eMF' :: String -> Int -> String -> Int -> String
{-
mS :: String -> the string of metafuck code
mD :: Int -> index of metafuck command we are on
bS :: String -> the string of brainfuck code
bD :: Int -> index of brainfuck command we on 
-}
eMF' mS mD bS bD
    | command == '+' 
        | bS !! bD == ',' = eMF' mS (succ mD) bS bD
        | otherwise = eMF' mS (succ mD) (bSPrev ++ [nextCommand] ++ bSPost) bD
                where nextCommand = (head $ tail $ dropWhile (/=(bS !! bD)) "+-<>[].,")
    | command == '-' 
        | bS !! bD == '+' = eMF' mS (succ mD) bS bD
        | otherwise = eMF' mS (succ mD) (bSPrev ++ [previousCommand] ++ bSPost) bD
                where previousCommand = (last $ takeWhile (/=(bS !! bD)) "+-<>[].,")
    | command == '>'
        | bD >= length bS = eMF' mS (succ mD) bS bD
        | otherwise = eMF' mS (succ mD) bS (succ bD)
    | command == '<'
        | bD == 0 = eMF' mS (succ mD) bS bD
        | otherwise = eMF' mS (succ mD) bS (pred bD)
    | command == '['
        | 
    | command == ']'
        | 
    | otherwise = eMF' (mSPrev ++ (funList !! (read (command:(takeWhile (\x -> elem x "0123456789") mS) :: Int))) ++ mSPost)
        where command = mS !! mD
              operand = bS !! bD
              bSPrev = fst $ splitAt bD bS
              bSPost = tail $ snd $ splitAt bD bS
              mSPrev = fst $ splitAt mD mS
              mSPost = tail $ snd $ splitAt mD mS
              seek[ count = 
              seek] = 