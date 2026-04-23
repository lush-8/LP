import Data.Ratio

exps :: Rational -> [Rational]
exps x = map fst (iterate step (1, 1))
    where
        step (terme, index) = (terme * x / index, index + 1)

exponencial :: Rational -> Rational -> Rational
exponencial x epsilon = sum (takeWhile (>= epsilon) (exps x))