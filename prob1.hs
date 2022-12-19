absValue :: Int -> Int
absValue n
  | n < 0 = -n
  | otherwise = n


power :: Int -> Int -> Int
power _ 0 = 1
power x n
  | even n = y * y
  | otherwise = y * y * x
  where
    y = power x divided
    divided = div n 2


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


slowFib :: Int -> Int
slowFib n
  | n == 0 = 0
  | n == 1 = 1
  | n >= 2 = slowFib (n-1) + slowFib (n-2)


auxQuick :: (Int, Int) -> Int -> Int
auxQuick (a,b) n
  | n == 2 = (a + b)
  | otherwise = auxQuick (b, (a + b)) (n-1)

quickFib :: Int -> Int
quickFib n
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = auxQuick (0,1) n
