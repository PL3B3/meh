data BFCommand = GoRight -- >
    | GoLeft -- <
    | Increment -- +
    | Decrement -- -
    | Print -- .
    | Read -- ,
    | LoopL -- [
    | LoopR -- ]
    | Comment Char -- anything else
    
type BFSource = [BFCommand]

parseBF :: String -> BFSource
parseBF = map charToBF
    where
        charToBF x = case x of
            '>' -> GoRight
            '<' -> GoLeft
            '+' -> Increment
            '-' -> Decrement
            '.' -> Print
            ',' -> Read
            '[' -> LoopL
            ']' -> LoopR
            c -> Comment c
        
