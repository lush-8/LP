import Control.Monad

main :: IO ()
main = do
    contingut <- getContents
    mapM_ print (processLines (lines contingut))
        where
            processLines :: [String] -> [Either String Int]
            processLines lines = map processOneLine lines
                where 
                    processOneLine :: String -> Either String Int
                    processOneLine line = evaluate (words line)

            evaluate :: [String] -> Either String Int
            evaluate expressio = extreureResultat (foldM step [] expressio)
                where
                    extreureResultat :: Either String [Int] -> Either String Int
                    extreureResultat (Right [res]) = Right res
                    extreureResultat (Left error) = Left error

                    step :: [Int] -> String -> Either String [Int]
                    step (y:x:pila) "+" = Right ((x + y) : pila)
                    step (y:x:pila) "-" 
                        | x < y = Left "neg"
                        | otherwise = Right ((x - y) : pila)
                    step (y:x:pila) "*" = Right ((x * y) : pila)
                    step (y:x:pila) "/" 
                        | y == 0 = Left "div0"
                        | x `mod` y /= 0 = Left "divE"
                        | otherwise = Right ((x `div` y) : pila)
                    step pila n = Right (read n : pila)