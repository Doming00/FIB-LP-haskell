getBMI :: [String] -> String
getBMI [n, we, he] = n ++ ": " ++ bmi
  where
    bmi :: String
    bmi
      | imc < 18 = "underweight"
      | imc <= 25 = "normal weight"
      | imc <= 30 = "overweight"
      | imc <= 40 = "obese"
      | otherwise = "severely obese"
      where
        imc :: Float
        imc = w / (h*h)
          where
            h, w :: Float
            h = read he
            w = read we


output :: String -> String
output [] = ""
output x = getBMI $ words x


main :: IO()
main = do
  line <- getLine
  if (line /= "*") then do
    putStrLn $ output line
    main
  else
    return()
