data Arbin = Buit | Node Int Arbin Arbin
    deriving Show

diametre :: Arbin -> (Int, Int)
diametre Buit = (0, 0)
diametre (Node _ l r) = (diameter, height)
    where
        (d1, h1) = diametre l
        (d2, h2) = diametre r
        height = 1 + max h1 h2
        diameter = max (h1 + h2) (max d1 d2)
        
convertirEnArbin :: [Int] -> Arbin
convertirEnArbin preodre = fst (helper preodre)

helper :: [Int] -> (Arbin, [Int])
helper [] = (Buit, [])
helper (-1:xs) = (Buit, xs)
helper (x:xs) = ((Node x l r), list)
    where
        (l, tmp) = helper xs
        (r, list) = helper tmp

main :: IO ()
main = do
    linia <- getLine
    
    if linia /= "" then do 
        let tree = convertirEnArbin (map read (words linia))
        putStrLn (show (fst (diametre (tree))))
        main
    else return ()