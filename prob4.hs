flatten :: [[Int]] -> [Int]
flatten xs = foldl (++) [] xs

myLength :: String -> Int
myLength xs = foldl (\y x -> y + 1) 0 xs


myReverse :: [Int] -> [Int]
myReverse xs = foldr (\x y -> y ++ [x]) [] xs

countIn :: [[Int]] -> Int -> [Int]
countIn xs y = map (\l -> length ( filter ( == y) l)) xs

firstWord :: String -> String
firstWord xs = takeWhile (/= ' ') (dropWhile (== ' ') xs)
