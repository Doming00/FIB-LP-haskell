ones :: [Integer]
ones = 1 : ones

nats :: [Integer]
nats = 0 : map (+1) nats

auxints 0 = 1
auxints x
  | x < 0 = (-1 * x) +1
  | otherwise = (-1 * x)

ints :: [Integer]
ints = iterate auxints 0

triangulars :: [Integer]
triangulars = 0 : next 0 1 where next t v = (t+v) : (next (t+v) (v+1))

factorials :: [Integer]
factorials = scanl (*) 1 [1..]

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

auxPrime :: Integer -> Integer -> Bool
auxPrime n x
  | (mod n x == 0) && (n /= x) = False
  | x > ceiling(sqrt (fromIntegral n)) = True
  | otherwise = auxPrime n (x+1)

isPrime :: Integer -> Bool
isPrime n
  | n == 0 = False
  | n == 1 = False
  | otherwise = auxPrime n 2

primes :: [Integer]
primes = [x | x <- [1..], isPrime x]


--hammings :: [Integer]


--lookNsay :: [Integer]


--tartaglia :: [[Integer]]
