import Data.Ratio

termes_cosinus :: Rational -> [Rational]
termes_cosinus alpha = generador 1 1
    where
        generador :: Rational -> Rational -> [Rational]
        generador terme n = terme : generador seguent (n + 1)
            where
                seguent :: Rational
                seguent = terme * (-(alpha * alpha)) / ((2 * n) * (2 * n - 1))

cosinus :: Rational -> Rational -> Rational
cosinus alpha epsilon = sum (takeWhile check (termes_cosinus alpha))
    where
        check :: Rational -> Bool
        check terme = abs terme >= epsilon