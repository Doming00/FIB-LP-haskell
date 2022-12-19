myMap :: (a -> b) -> [a] -> [b]
myMap f xs = [q | x <- xs, let q = f x]


myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f xs = [x | x <- xs, f x]



myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f xs ys = [z | (x, y) <- zip xs ys, let z = f (fst (x , y)) (snd (x, y))]




thingify :: [Int] -> [Int] -> [(Int, Int)]
thingify xs ys = [z | x <- xs, y <- ys, mod x y == 0, let z = (x, y)]



factors :: Int -> [Int]
factors a = [x | x <- [1..a], mod a x == 0]
