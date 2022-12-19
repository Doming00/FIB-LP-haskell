myLength :: [Int] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs


myMaximum :: [Int] -> Int
myMaximum [x] = x
myMaximum (x:y:xs)
  | x >= y = myMaximum ([x] ++ xs)
  | otherwise = myMaximum ([y] ++ xs)


average :: [Int] -> Float
average xs = (fromIntegral (sum xs)) / (fromIntegral (myLength xs))


buildPalindrome :: [Int] -> [Int]
buildPalindrome [] = []
buildPalindrome xs = (reverse xs) ++ xs


auxRemove :: [Int] -> Int -> [Int]
auxRemove [] _ = []
auxRemove (x:xs) y
  | x == y = auxRemove xs y
  | otherwise = [x] ++ (auxRemove xs y)

remove :: [Int] -> [Int] -> [Int]
remove [] _ = []
remove xs [] = xs
remove xs (y:ys) = remove (auxRemove xs y) ys



flatten :: [[Int]] -> [Int]
flatten [] = []
flatten (x:xs) = x ++ flatten xs



odds :: [Int] -> [Int]
odds [] = []
odds (x:xs)
  | odd x = [x] ++ odds xs
  | otherwise = odds xs

evens :: [Int] -> [Int]
evens [] = []
evens (x:xs)
  | even x = [x] ++ evens xs
  | otherwise = evens xs

oddsNevens :: [Int] -> ([Int],[Int])
oddsNevens x = (odds x, evens x)


auxPrime :: Int -> Int -> Bool
auxPrime n x
  | (mod n x == 0) && (n /= x) = False
  | x > ceiling(sqrt (fromIntegral n)) = True
  | otherwise = auxPrime n (x+1)

isPrime :: Int -> Bool
isPrime n
  | n == 0 = False
  | n == 1 = False
  | otherwise = auxPrime n 2

primeDivisors :: Int -> [Int]
primeDivisors x = [y | y <-[0..x], isPrime(y) && mod x y == 0]
