module LinearAlgebra (
Tensor,
shapeOf,
switchDimensions,
toIndex,
toCoordinate
) where

import Debug.Trace
import Data.List

data Tensor a = Tensor [Int] [a] deriving Show

bob = Tensor [2,3] [1..6]
iri = Tensor [3,2] [1..6]

shapeOf :: Tensor a -> [Int]
shapeOf (Tensor shape info) = shape

infoOf :: Tensor a -> [a]
infoOf (Tensor shape info) = info

splitInto :: [a] -> Int -> [[a]]
splitInto list size
    | length list == 0 = []  
    | otherwise = [take size list] ++ splitInto (drop size list) size

mv :: (Num a) => [a] -> [a] -> a
mv v1 v2 = sum $ zipWith (*) v1 v2 

pp = pred . product

switchDimensions :: Tensor a -> [Int] -> Tensor a
switchDimensions (Tensor shape info) flipDimensions = Tensor newShape newInfo
    where newShape = flip flipDimensions shape
          newInfo = map (\a -> info !! (toIndex (flip flipDimensions (toCoordinate a newShape)) shape)) [0..pp newShape]
          flip fDims l = map (\a -> l !! a) fDims

valueAtCoordinate :: Tensor a -> [Int] -> a
valueAtCoordinate (Tensor shape info) coordinate = info !! (toIndex coordinate shape)
    
toIndex :: [Int] -> [Int] -> Int
toIndex coordinate dimensions = mv coordinate sizes
    where sizes = (map (\a -> product $ drop a dimensions) [1..(pred $ length dimensions)]) ++ [1]

toCoordinate :: Int -> [Int] -> [Int]  
toCoordinate index dimensions
    | length dimensions == 0 = []
    | otherwise = [div index dTail] ++ toCoordinate (mod index dTail) (tail dimensions)
        where dTail = product $ tail dimensions

--simple 2d matrix multiplication    
mm :: (Num a) => Tensor a -> Tensor a -> Tensor a
mm (Tensor shape1 info1) t2@(Tensor shape2 info2) = Tensor [dim1, dim2] (zipWith mv (splitInto info1 dim1) (splitInto (infoOf $ switchDimensions t2 [1,0]) dim2))
    where dim1 = shape1 !! 0
          dim2 = shape2 !! 1

--tensor contraction. multiple of all the elements. ahh
tc :: (Num a, Show a) => Tensor a -> Tensor a -> Int -> Tensor a
tc (Tensor shape1 info1) (Tensor shape2 info2) overlap = Tensor newShape newInfo
        where newShape = (take ((length shape1) - overlap) shape1) ++ (drop overlap shape2)
              getValue newCoordinates = (sum $ (zipWith (*) (map (\a -> info1 !! (toIndex a shape1)) (frontCoordinates newCoordinates)) (map (\a -> info2 !! (toIndex a shape2)) (backCoordinates newCoordinates))))
              frontCoordinates newCoordinates = (map (\a -> (take halfLength newCoordinates) ++ toCoordinate a backShape1) [0..(pred $ product backShape1)])
              backCoordinates newCoordinates = (map (\a -> (reverse $ toCoordinate a frontShape2) ++ (drop halfLength newCoordinates)) [0..(pred $ product frontShape2)])
              backShape1 = (drop ((length shape1) - overlap) shape1)
              frontShape2 = take overlap shape2
              newInfo = map (\newInfoIndex -> getValue $ toCoordinate newInfoIndex newShape) [0..(pred $ product newShape)]
              halfLength = div (length newShape) 2
          
