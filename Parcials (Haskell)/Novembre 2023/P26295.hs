import Data.List (sort, group)

main :: IO ()
main = do
    contingut <- getContents
    mapM_ putStrLn (processar (words contingut))
        where
            processar :: [String] -> [String]
            processar lletres = map output (group (sort lletres))

            output :: [String] -> String
            output agrupats = head agrupats ++ " " ++ show (length agrupats)