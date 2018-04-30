module Grub () where

import Data.List
import Data.List.Split
import Data.Maybe
import Data.Char
import System.Random
--import Lang as L

funList = ["+","-","<",">","[","]",".",","]

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
mS is a string of code called metafuck which uses brainfuck symbols and applies them to brainfuck, similar to how a string of brainfuck applies to a string of numbers (a tape)
mS :: String -> the string of metafuck code
mD :: Int -> index of metafuck command we are on
bS :: String -> the string of brainfuck code
bD :: Int -> index of brainfuck command we on 
-}
eMF' mS mD bS bD = if (mD == length mS) then bS else case (mS !! mD) of 
    '+' -> if (elem operand ", ") then eMF' mS (succ mD) bS bD else eMF' mS (succ mD) (bSPrev ++ [nextCommand] ++ bSPost) bD
                where nextCommand = (head $ tail $ dropWhile (/=(operand)) "+-<>[].,")
    --The plus function, incrementing a BF command one up in the preset scale
    '-' -> if (elem operand "+ ") then eMF' mS (succ mD) bS bD else eMF' mS (succ mD) (bSPrev ++ [previousCommand] ++ bSPost) bD
                where previousCommand = (last $ takeWhile (/=(operand)) "+-<>[].,")
    --Like plus, but minus
    '>' -> if (bD == pred (length bS) || length bS == 0) then eMF' mS (succ mD) bS bD else eMF' mS (succ mD) bS (succ bD)
    --Move pointer one right in the BF code
    '<' -> if (bD == 0) then eMF' mS (succ mD) bS bD else eMF' mS (succ mD) bS (pred bD)
    --Move pointer one left in the BF code
    '[' -> if (operand == '+') then eMF' mS (mD + (length $ takeBalancedBrackets mSPost 1)) bS bD else eMF' mS (succ mD) bS bD
    --Left loop. If BF operand equals '+' (Zero), then skip the next matching right bracket, else evaluate code after leftbracket as normal
    ']' -> if (operand /= '+') then eMF' mS (mD - (length $ takeBalancedBrackets (reverse mSPrev) (-1))) bS bD else eMF' mS (succ mD) bS bD
    --Right loop. If BF operand ISN'T '+' (Zero), then go back to matching left bracket and evaluate code up until it comes back to you (you being the ]), else skip
    '.' -> eMF' mS (succ mD) (bSPrev ++ [command] ++ (operand:bSPost)) bD
    --Print function. Insert current command in current BF index position
    ',' -> eMF' (mSPrev ++ [operand] ++ (command:mSPost)) (succ mD) bS bD
    --Read function. GET BF thing and put in into our mS code
    --'*' -> if (operand == ' ') then eMF' mS (succ mD) bS bD else eMF' mS (succ mD) (bSPrev ++ bSPost) bD
    --Delete function 
    --Given a numerical sequence, match it to the index of a function in the functionlist, and replace it with that function.
    where command = mS !! mD
          operand
            | length bS == 0 = ' ' -- in order to handle cases where bS is a blank string
            | otherwise = bS !! bD -- operand is just the bS character we operate on
          bSPrev = fst $ splitAt bD bS
          bSPost
            | bD == length bS = []
            | otherwise = tail $ snd $ splitAt bD bS -- all BF code after the operand
          mSPrev = fst $ splitAt mD mS
          mSPost = tail $ snd $ splitAt mD mS -- all mS code after the command
          takeBalancedBrackets (x:xs) blnc -- Given a list and a "balance" (equaling left [ - right ] brackets), take from list until balance is zero
            | blnc == 0 = []
            | xs == [] = []
            | x == '[' = x:(takeBalancedBrackets xs $ succ blnc)
            | x == ']' = x:(takeBalancedBrackets xs $ pred blnc)
            | otherwise = x:(takeBalancedBrackets xs blnc)



--createMetaFuck :: ([Int],[Int]) -> Int -> Int -> [(String, Float)] -> (([Int],[Int]) -> Float) -> String
--createMetaFuck data@(inList, outList) codeLength numNew oldGen@((code, fitness):xs) costFunc = 

genRandomCode codeLength num = do
    gen <- getStdGen
    let randLets = chunksOf codeLength $ take (codeLength * (num - 1)) (randomRs ('a','h') gen)
    newGen <- newStdGen
    let randLetsEnd = [take codeLength $ randomRs ('a','h') newGen]
    let bfStrs = (process $ randLets ++ randLetsEnd)
    return bfStrs
        where process = map (\l -> map (\x -> "+-<>[].," !! (head $ findIndices (==x) "abcdefgh")) l) 

balanceBrackets' (x:xs) balance
    | not $ elem x "[]" = x:(balanceBrackets' xs balance) 
    | x == '[' = x:(balanceBrackets' xs (succ balance))
    | otherwise = x:(balanceBrackets' xs (succ balance))

startWithLeftBracket (x:xs)
    | not $ elem x "[]" = x:(startWithLeftBracket xs)
    | x == ']' = '[':(startWithLeftBracket xs)
    | otherwise = x:xs

evenOutBrackets (x:xs) = if (length $ )
