module Grub () where

import Data.List
import Data.Maybe
import Data.Char
--import Lang as L

funList = []

main = do
  putStrLn "Put meta String"
  ms <- getLine
  putStrLn "Put BF String"
  bs <- getLine
  putStrLn $ evalMetaFuck ms bs

--costFunc :: [L.Tape a] -> (L.Tape a -> L.Tape a) -> (L.Tape a -> L.Tape a -> Int) -> ([Int] -> Float) -> Float
--costFunc testList funcToTest errorFunc cumeFunc = cumeFunc $ zipWith errorFunc testList (map funcToTest testList)

evalMetaFuck mStr bStr = eMF' mStr 0 bStr 0

evalLegitMetaFuck :: String -> String -> Maybe String
evalLegitMetaFuck mStr bStr
    | count '[' mStr /= count ']' mStr = Nothing 
    | otherwise = Just (eMF' mStr 0 bStr 0)
        where count a = length . (filter (==a)) 

eMF' :: String -> Int -> String -> Int -> String
{-
mS :: String -> the string of metafuck code
mD :: Int -> index of metafuck command we are on
bS :: String -> the string of brainfuck code
bD :: Int -> index of brainfuck command we on 
-}
eMF' mS mD bS bD = if (mD == length mS) then bS else case (mS !! mD) of 
    '+' -> if (operand == ',') then eMF' mS (succ mD) bS bD else eMF' mS (succ mD) (bSPrev ++ [nextCommand] ++ bSPost) bD
                where nextCommand = (head $ tail $ dropWhile (/=(operand)) "+-<>[].,")
    '-' -> if (operand == '+') then eMF' mS (succ mD) bS bD else eMF' mS (succ mD) (bSPrev ++ [previousCommand] ++ bSPost) bD
                where previousCommand = (last $ takeWhile (/=(operand)) "+-<>[].,")
    '>' -> if (bD == pred (length bS)) then eMF' mS (succ mD) bS bD else eMF' mS (succ mD) bS (succ bD)
    '<' -> if (bD == 0) then eMF' mS (succ mD) bS bD else eMF' mS (succ mD) bS (pred bD)
    '[' -> if (operand == '+') then eMF' mS (mD + (length $ takeBalancedBrackets mSPost 1)) bS bD else eMF' mS (succ mD) bS bD 
    ']' -> if (operand /= '+') then eMF' mS (mD - (length $ takeBalancedBrackets (reverse mSPrev) (-1))) bS bD else eMF' mS (succ mD) bS bD
    _ -> eMF' (mSPrev ++ (funList !! (read (command:(takeWhile (\x -> elem x "0123456789") mS)) :: Int) ++ (dropWhile (\x -> elem x "0123456789") mSPost))) mD bS bD
    where command = mS !! mD
          operand = bS !! bD
          bSPrev = fst $ splitAt bD bS
          bSPost = tail $ snd $ splitAt bD bS
          mSPrev = fst $ splitAt mD mS
          mSPost = tail $ snd $ splitAt mD mS
          takeBalancedBrackets (x:xs) blnc
            | blnc == 0 = []
            | xs == [] = []
            | x == '[' = x:(takeBalancedBrackets xs $ succ blnc)
            | x == ']' = x:(takeBalancedBrackets xs $ pred blnc)
            | otherwise = x:(takeBalancedBrackets xs blnc)