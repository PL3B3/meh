module MatrixMaths
(
  Matrix,
  Coord,
  matrix,
  scalarMult,
  transposeMulti,
  unwrapCoordinate,
  dotVector,
  indexOfCoordinate,
  coordinateOfIndex,
  valOfCoordinate,
  addMatrix,
  matrixByVec,
  hMult,
  multMatrix,
  chunksOf,
  transpose,
  valsFromMatrix
) where

--Matrix consists of a list of integers which are the dimensions (x, y, z, and more) and a bunch of floats which are the data
data Matrix a = Matrix [Int] [a] deriving (Eq, Show)

--Order is (x, y, z) dimensions from lowest to highest, starting from ROW, then COL, then PLANE, then RECT, etc. 
data Coord = Coord [Int] deriving (Eq, Show)

tm = Matrix [3,3,3] [11,100,46,14,64,22,2,7,91,80,99,69,29,74,92,55,14,48,19,9,38,31,62,38,95,42,87]
tc = Coord [2,1,1]

tm1 = Matrix [2,3] [1.0,2.0,3.0,4.0,5.0,6.0]
tm2 = Matrix [3,2] [7.0,8.0,9.0,10.0,11.0,12.0]

matrix :: [Int] -> [a] -> Matrix a
matrix a b = Matrix a b

--Multiply everything in a matrix by some number
scalarMult :: (Num a) => Matrix a -> a -> Matrix a
scalarMult (Matrix dimensions values) num = Matrix dimensions (map (*num) values)

--splits into chunks
chunksOf :: Int -> [a] -> [[a]]
chunksOf num list
  | length list == 0 = []
  | otherwise = (take num list):(chunksOf num (drop num list))

--Transpose 2 dimensions. point with coordinate (a,b,c) transposing dimensions 0 and 2, becomes (c,b,a)
transposeMulti :: Matrix a -> Int -> Int -> Matrix a
transposeMulti matrix@(Matrix dimensions values) firstDimension secondDimension = Matrix (switch dimensions f s) newvals
  where switch list a b = (take a list) ++ [list !! b] ++ (take (pred $ b - a) (drop (succ a) list)) ++ [list !! a] ++ drop (succ b) list 
        f = if firstDimension < secondDimension then firstDimension else secondDimension
        s = if secondDimension > firstDimension then secondDimension else firstDimension
        positions = foldl (\new val -> new ++ [indexOfCoordinate (Matrix (switch dimensions f s) values) (Coord (switch (unwrapCoordinate (coordinateOfIndex matrix val)) f s))]) [] [0..(pred $ length values)]
        newvals = map (\x -> values !! x) positions

transpose :: Matrix a -> Matrix a
transpose (Matrix dimensions values) = Matrix (reverse dimensions) (foldl (\x y -> x ++ (y (chunksOf (dimensions !! 0) values))) [] funkies)
  where funkies = reverse $ foldl (\x y -> (\g -> map (\a -> a !! y) g):x) [] [0..(pred $ dimensions !! 0)]

--Coordinate without the CLUNK
unwrapCoordinate :: Coord -> [Int]
unwrapCoordinate (Coord a) = a

--A little goof, a little gaff, a little DOTPRODUCT OF ONE DIMENSIONAL VECTORS
dotVector :: (Num a) => [a] -> [a] -> a
dotVector a b = sum $ zipWith (*) a b

--Takes a matrix and converts a coordinate to whatever index it represents within the matrix
indexOfCoordinate :: Matrix a -> Coord -> Int
indexOfCoordinate (Matrix dimensions values) (Coord coords) = dotVector coords layercounts
  where layercounts = 1:(reverse $ map (\x -> product $ drop x (reverse dimensions)) [1..(pred $ length dimensions)])

--Opposite of indexOfCoordinate
coordinateOfIndex :: Matrix a -> Int -> Coord
coordinateOfIndex (Matrix dimensions values) num = Coord (tail $ foldl (\x@(a:as) y -> (mod a y):(div a y):as) [num] layercounts)
  where layercounts = map (\x -> product $ drop x (reverse dimensions)) [1..(pred $ length dimensions)] ++ [1]

--It's S E L F E X P L A N A T O R Y
valsFromMatrix :: Matrix a -> [a]
valsFromMatrix (Matrix dims vals) = vals

--Gets the value at a coordinate
valOfCoordinate :: Matrix a -> Coord -> a
valOfCoordinate matrix@(Matrix dimensions values) coord = values !! (indexOfCoordinate matrix coord)

--Adds each element of the matrices
addMatrix :: (Num a) => Matrix a -> Matrix a -> Matrix a
addMatrix (Matrix a b) (Matrix x y) = Matrix a (zipWith (+) b y)

--Multiply a matrix by a vector!!
matrixByVec :: (Num a) => Matrix a -> [a] -> [a]
matrixByVec m@(Matrix dims values) vec = map (\x -> dotVector x vec) ((\a@(Matrix b c) -> chunksOf (length vec) c) (transposeMulti m 0 1))

--Multiply two matrices
multMatrix :: (Num a) => Matrix a -> Matrix a -> Matrix a
multMatrix m1@(Matrix dims1 vals1) m2@(Matrix dims2 vals2) = transpose newVals
  where transp = transpose m1
        cols1 = chunksOf (dims1 !! 1) (valsFromMatrix transp)
        cols2 = chunksOf (dims2 !! 0) vals2
        newVals = Matrix [(dims1 !! 0), (dims2 !! 1)] (foldl (\x y -> x ++ (map (\a -> dotVector a y) cols2)) [] cols1)
--
hMult :: (Num a) => [a] -> [a] -> [a]
hMult a b = zipWith (*) a b
