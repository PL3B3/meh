import Control.Monad
import Data.Char

main = operate upAll

operate f = do
  contents <- getContents
  putStr $ f contents

upAll = map toUpper

spongeboblify t = unwords (map (map (\x -> if elem x "aeiouypstg" then toUpper x else x)) (words t))
