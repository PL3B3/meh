module LinearAlgebra (
Tensor,
shapeOf,
switchDimensions,
toIndex,
toCoordinate
) where

data Tensor a = Tensor [Int] [a]

shapeOf :: Tensor a -> [Int]
shapeOf (Tensor shape info) = shape

mv :: (Num a) => [a] -> [a] -> a
mv v1 v2 = sum $ zipWith (*) v1 v2 

switchDimensions :: Tensor a -> [Int] -> Tensor a
switchDimensions (Tensor shape info) flipDimensions = Tensor newShape info
    where newShape = map (\a -> shape !! a) flipDimensions
          
toIndex :: [Int] -> [Int] -> Int
toIndex coordinate dimensions = mv coordinate sizes
    where sizes = (map (\a -> product $ drop a dimensions) [1..(pred $ length dimensions)]) ++ [1]

toCoordinate :: Int -> [Int] -> [Int]  
toCoordinate index dimensions
    | length dimensions == 0 = []
    | otherwise = [div index dTail] ++ toCoordinate (mod index dTail) (tail dimensions)
        where dTail = product $ tail dimensions

--tensor contraction. multiple of all the elements. ahh
tc :: Tensor a -> Tensor a -> Int -> Tensor a
tc (Tensor shape1 info1) (Tensor shape2 info2) overlap = Tensor newShape newInfo
    where newShape = ((take ((length shape1) - overlap) shape1) ++ (drop overlap shape2))
          newInfo = foldl (\values, newInfoIndex -> values ++ (toCoordinate newInfoIndex newShape)) [] [0..(pred $ product newShape)]
          
