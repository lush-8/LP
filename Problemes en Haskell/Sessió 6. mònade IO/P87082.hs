main :: IO ()
main = do
    line <- getLine

    if line /= "*"
        then do
            putStrLn (evaluateIMC line)
            main
        else return ()

evaluateIMC :: String -> String
evaluateIMC line = nom ++ ": " ++ interpretacio
    where
        [nom, pesStr, alturaStr] = words line

        pes = read pesStr :: Double

        altura = read alturaStr :: Double

        imc = pes / (altura * altura)

        interpretacio
            | imc < 18 = "magror"
            | imc < 25 = "corpulencia normal"
            | imc < 30 = "sobrepes"
            | imc < 40 = "obesitat"
            | otherwise = "obesitat morbida"