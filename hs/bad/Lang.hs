module Lang (BFCommand, BFSource, BFSequence, Tape, parseBF, checkBF, checkBF', checkBF'') where
    
import Data.Maybe
import Data.Char

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

runBF :: BFSource -> IO ()
runBF = run emptyTape . bfSourceToTape
  where bfSourceToTape [] = Tape [] Comment []
        bfSourceToTape (b:bs) = Tape [] b bs

run :: Tape Int -> Tape BFCommand -> IO ()
run dataTape source@(Tape _ GoRight _) = advance (moveRight dataTape) source
run dataTape source@(Tape _ GoLeft _) = advance (moveLeft dataTape) source
run (Tape l p r) source@(Tape _ Increment _) = advance (Tape l (p+1) r) source
run (Tape l p r) source@(Tape _ Decrement _) = advance (Tape l (p-1) r) source
run dataTape@(Tape _ p _) source@(Tape _ Print _) = do
  putChar (chr p)
  hFlush stdout
  advance dataTape source
run dataTape@(Tape _ p _) source@(Tape _ Print _) = do
  p <- getChar
  advance (Tape l (ord p ) r) source
run dataTape@(Tape _ p _) source@(Tape _ LoopL _)
  | p == 0 = seekLoopR 0 dataTape source
  | otherwise = advance dataTape source
run dataTape@(Tape _ p _) source@(Tape _ LoopR _)
  | p /= 0 = seekLoopL 0 dataTape source
  | otherwise = advance dataTape source
run dataTape source@(Tape _ (Comment _) _) = advance dataTape source

seekLoopR :: Int -> Tape Int -> Tape BFCommand -> IO ()
seekLoopR 1 dataTape source@(Tape _ LoopR _) = advance dataTape source
seekLoopR b dataTape source@(Tape _ LoopR _) = seekLoopR (b-1) dataTape (moveRight source)
seekLoopR b dataTape source@(Tape _ LoopL _) = seekLoopR (b+1) dataTape (moveRight source)
seekLoopR b dataTape source = seekLoopR b dataTape (moveRight source)

seekLoopL :: Int -> Tape Int -> Tape BFCommand -> IO ()
seekLoopL 1 dataTape source@(Tape _ LoopL _) = advance dataTape source
seekLoopL b dataTape source@(Tape _ LoopL _) = seekLoopL (b-1) dataTape (moveLeft source)
seekLoopL b dataTape source@(Tape _ LoopR _) = seekLoopL (b+1) dataTape (moveLeft source)
seekLoopL b dataTape source = seekLoopL b dataTape (moveLeft source)


advance :: Tape Int -> Tape BFCommand -> IO ()
advance dataTape (Tape _ _ []) = return ()
advance dataTape source = run dataTape (moveRight source)
