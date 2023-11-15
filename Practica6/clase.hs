getLine' = do c <- getChar
              if c == '\n'
              then return ""
              else do s <- getLine'
                      return (c:s)

ahorcado :: IO ()
ahorcado =
    do putStrLn "Piense en una palabra"
       palabra <- sgetLine  
       putStrLn
       return palabra

{-

sgetLine :: IO String
sgetLine - do SetEcho stdin False
              palabra <- sgetLine'
              hSetEcho stdin True
              return palabra

sgetLine' :: IO String
sgetline' = do x <- getChar
               if x == '

-}