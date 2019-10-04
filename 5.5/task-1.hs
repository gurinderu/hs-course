main' :: IO ()
main'
  = do name <- helper
       putStrLn $ "Hi, " ++ name ++ "!"
  where helper :: IO String
        helper
          = do putStrLn "What is your name?"
               putStr "Name: "
               name <- getLine
               if name == "" then helper else return name
