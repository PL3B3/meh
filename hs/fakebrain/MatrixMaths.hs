module MatrixMaths
(
  --dot,
  --transpose,
  --add
) where


--Matrix consists of a list of integers which are the dimensions (x, y, z, and more) and a bunch of floats which are the data
data Matrix = Matrix [Int] [Double] deriving (Eq, Show)

--Order is (x, y, z) dimensions from lowest to highest, starting from ROW, then COL, then PLANE, then RECT, etc. 
data Coord = Coord [Int] deriving (Eq, Show)

tm = Matrix [3,3,3] [11,100,46,14,64,22,2,7,91,80,99,69,29,74,92,55,14,48,19,9,38,31,62,38,95,42,87]
tc = Coord [2,1,1]


--Multiply everything in a matrix by some number
scalarMult :: Matrix -> Double -> Matrix
scalarMult (Matrix dimensions values) num = Matrix dimensions (map (*num) values)

--Transpose 2 dimensions. point with coordinate (a,b,c) transposing dimensions 0 and 2, becomes (c,b,a)
transpose :: Matrix -> Int -> Int -> Matrix
transpose matrix@(Matrix dimensions values) firstDimension secondDimension = Matrix (switch dimensions f s) newvals
  where switch list a b = (take a list) ++ [list !! b] ++ (take (pred $ b - a) (drop (succ a) list)) ++ [list !! a] ++ drop (succ b) list 
        f = if firstDimension < secondDimension then firstDimension else secondDimension
        s = if secondDimension > firstDimension then secondDimension else firstDimension
        positions = foldl (\new val -> new ++ [indexOfCoordinate (Matrix (switch dimensions f s) values) (Coord (switch (unwrapCoordinate (coordinateOfIndex matrix val)) f s))]) [] [0..(pred $ length values)]
        newvals = map (\x -> values !! x) positions

--Coordinate without the CLUNK
unwrapCoordinate :: Coord -> [Int]
unwrapCoordinate (Coord a) = a

--A little goof, a little gaff, a little DOTPRODUCT OF ONE DIMENSIONAL VECTORS
dotVector :: (Num a) => [a] -> [a] -> a
dotVector a b = sum $ zipWith (*) a b

--Takes a matrix and converts a coordinate to whatever index it represents within the matrix
indexOfCoordinate :: Matrix -> Coord -> Int
indexOfCoordinate (Matrix dimensions values) (Coord coords) = dotVector coords layercounts
  where layercounts = 1:(reverse $ map (\x -> product $ drop x (reverse dimensions)) [1..(pred $ length dimensions)])

--Opposite of indexOfCoordinate
coordinateOfIndex :: Matrix -> Int -> Coord
coordinateOfIndex (Matrix dimensions values) num = Coord (tail $ foldl (\x@(a:as) y -> (mod a y):(div a y):as) [num] layercounts)
  where layercounts = map (\x -> product $ drop x (reverse dimensions)) [1..(pred $ length dimensions)] ++ [1]
