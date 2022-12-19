eql :: [Int] -> [Int] -> Bool
eql [][] = True
eql _ [] = False
eql [] _ = False
eql (x:xs) (y:ys)
  | x == y = eql xs ys
  | otherwise = False


prod :: [Int] -> Int
prod xs = foldl (*) 1 xs

prodOfEvens :: [Int] -> Int
prodOfEvens xs = prod (filter even xs)


powersOf2 :: [Int]
powersOf2 = iterate (2*) 1


scalarProduct :: [Float] -> [Float] -> Float
scalarProduct [] [] = 0
scalarProduct (x:xs) (y:ys) = x*y + scalarProduct xs ys
