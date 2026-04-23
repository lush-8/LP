data Arbin = Buit | Node Int Arbin Arbin
    deriving Show

escorar :: Arbin -> (Arbin, Int, Int)
escorar Buit = (Buit, 0, 0)
escorar (Node val esq dre)
    | check = (Node val nouDre nouEsq, pes, mida)
    | otherwise = (Node val nouEsq nouDre, pes, mida)
    where
        check = pesEsq > pesDre || (pesEsq == pesDre && midaEsq > midaDre)

        (nouEsq, pesEsq, midaEsq) = escorar esq

        (nouDre, pesDre, midaDre) = escorar dre

        pes = val + pesEsq + pesDre

        mida = 1 + midaEsq + midaDre 

llegirArbin :: [Int] -> (Arbin, [Int])
llegirArbin [] = (Buit, [])
llegirArbin (x:xs)
    | x == -1 = (Buit, xs)
    | otherwise = (Node x esq dre, rest)
    where  
        (esq, tmp) = llegirArbin xs
        (dre, rest) = llegirArbin tmp
        
convertirEnArbin :: [Int] -> Arbin
convertirEnArbin = fst . llegirArbin

main :: IO ()
main = do
    content <- getContents
    processLines (lines content)
        where
            processLines :: [String] -> IO ()
            processLines [] = return ()
            processLines (line:rest) = do
                print arbreEscorat
                processLines rest
                    where
                        llistaPreordre = map read (words line)

                        arbre = convertirEnArbin llistaPreordre

                        (arbreEscorat, _, _) = escorar arbre