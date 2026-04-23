import Text.Read (readMaybe)
import Control.Monad (join, foldM)

main :: IO ()
main = getContents >>= print . process
    where
        process input = calcula (map readMaybe (words input))
    
calcula :: [Maybe Int] -> Maybe (Int, Int)
calcula [] = Nothing
calcula (x:xs) = join (fmap plega x)
    where
        plega v = foldM actualitza (v, v) xs

        actualitza (maxim, minim) (Just n) = Just (max maxim n, min minim n)
        actualitza _ Nothing = Nothing