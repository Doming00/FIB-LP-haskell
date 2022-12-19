

main = do
  name <- getLine
  if (head name) == 'A' || (head name) == 'a' then do
    putStrLn "Hello!"
  else putStrLn "Bye!"
