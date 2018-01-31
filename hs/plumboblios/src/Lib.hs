module Lib
    ( someFunc
    ) where

import Data.Char  

someFunc :: IO ()
someFunc = do 
  putStrLn "IT'S TIME TO DO A GAME. GIVE ME A NUMBER"
  num <- getLine 
  putStrLn ("HAHA I FOOLED YOU, THE NUMBER " ++ num ++ " IS MEANINGLESS")

main = do   
    line <- getLine  
    if null line  
        then return ()  
        else do  
            putStrLn $ spongeBobify $ flippo line
            main  

flippo :: String -> String
flippo = unwords . reverse . words

reverseWords :: String -> String  
reverseWords = unwords . map reverse . words  

spongeBobify :: String -> String
spongeBobify str = map spongu (map toLower str)
  where spongu x
          | elem x ['a','e','i','o','u','n','y','r','g','b'] = toUpper x
          | otherwise = x 