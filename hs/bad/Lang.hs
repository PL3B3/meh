module Bfuck (BFCommand, BFSource, BFSequence, parseBF, checkBF, checkBF', checkBF'') where
    
import Data.Maybe

data BFCommand = GoRight -- >
    | GoLeft -- <
    | Increment -- +
    | Decrement -- -
    | Print -- .
    | Read -- ,
    | LoopL -- [
    | LoopR -- ]
    | Comment Char -- anything else
    deriving (Show, Eq, Read)
    
type BFSource = [BFCommand]

data BFSequence = BFSequence [BFCommand] deriving (Show, Eq, Read)

data Tape a = Tape [a] a [a]

--instance Show BFSequence where
    

parseBF :: String -> BFSource
parseBF = mapMaybe charToBF
    where
        charToBF x = case x of
            '>' -> Just GoRight
            '<' -> Just GoLeft
            '+' -> Just Increment
            '-' -> Just Decrement
            '.' -> Just Print
            ',' -> Just Read
            '[' -> Just LoopL
            ']' -> Just LoopR
            c -> Nothing
        
checkBF :: BFSequence -> Either String BFSequence
checkBF (BFSequence bfseq)
    | lc == rc = Right (BFSequence bfseq)
    | lc > rc = Left ("Mismatched brackets: " ++ (show (lc - rc)) ++ " too many Left Brackets") 
    | otherwise = Left ("Mismatched brackets: " ++ (show (rc - lc)) ++ " too many Right Brackets")
        where count x = length. filter (==x)
              lc = count LoopL bfseq
              rc = count LoopR bfseq

checkBF'' :: BFSequence -> Int
checkBF'' (BFSequence bfseq) = rc - lc
    where count x = length. filter (==x)
          lc = count LoopL bfseq
          rc = count LoopR bfseq

right :: Either a b -> b
right (Right b) = b

left :: Either a b -> a
left (Left a) = a


checkBF' :: String -> Maybe BFSequence
checkBF' a
    | lc == rc = Just (BFSequence bfseq)
    | otherwise = Nothing
        where count x = length. filter (==x)
              lc = count LoopL bfseq
              rc = count LoopR bfseq
              bfseq = parseBF a

emptyTape :: Tape Int
emptyTape = Tape zeros 0 zeros
    where zeros = repeat 0

moveRight :: Tape a -> Tape a
moveRight (Tape ls p []) = Tape ls p []
moveRight (Tape ls p (r:rs)) = Tape (p:ls) r rs

moveLeft :: Tape a -> Tape a
moveLeft (Tape [] p rs) = Tape [] p rs
moveLeft (Tape (l:ls) p rs) = Tape ls l (p:rs)

removeNum :: (Eq a) => [a] -> a -> Int -> [a]
removeNum (x:xs) rm n
    | n == 0 = (x:xs)
    | xs == [] && x == rm = []
    | xs == [] && x /= rm = [x]
    | x == rm = removeNum xs rm (n - 1)
    | otherwise = x:(removeNum xs rm n)
    
makeLegit :: String -> String
makeLegit code
    | bias == 0 = code  
    | bias < 0 = removeNum code '[' (-(bias))
    | otherwise = reverse (removeNum (reverse code) ']' bias)
        where bias = checkBF'' (BFSequence $ parseBF code)
