main :: IO ()
main = do
    contingut <- getContents
    putStrLn (show (process contingut)) 
        
process :: String -> Int
process input = sum (map read (words input))