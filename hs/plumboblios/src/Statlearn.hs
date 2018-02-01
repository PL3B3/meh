module Statlearn
    (printboi
	) where 

printboi :: IO ()
printboi = putStrLn "YOU ARE BAD"

mse :: [Float] -> [[Float]] -> ([Float] -> Float) -> Float
mse y x fhat = (1 / (fromIntegral (length y))) * (sum $ zipWith (\x y -> (x - y)^2) y (map fhat x))


