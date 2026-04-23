data Temps = Temps Int Int

data Arbre a = Arbre a [Arbre a]

instance Show Temps where
    show (Temps h m) = pad h ++ ":" ++ pad m
        where
            pad x
                | x < 10 = "0" ++ show x
                | otherwise = show x

suma :: Temps -> Temps -> Temps
suma (Temps h1 m1) (Temps h2 m2) = Temps hf mf
    where
        minuts = m1 + m2

        mf = minuts `mod` 60

        hf = h1 + h2 + (minuts `div` 60)

sumes :: [Temps] -> Temps
sumes = foldr suma (Temps 0 0)

sumesArbre :: Arbre Temps -> Temps
sumesArbre (Arbre arrel fills) = foldr suma arrel (map sumesArbre fills)

main :: IO ()
main = do
    contingut <- getContents
    processar (lines contingut)
        where
            processar [] = return ()
            processar (linia:_) = print (sumes (map parseTemps (words linia)))

            parseTemps str = Temps (read h) (read m)
                where
                    h = takeWhile (/= ':') str

                    m = tail (dropWhile (/= ':') str) 