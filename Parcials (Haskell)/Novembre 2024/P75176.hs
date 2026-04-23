import Data.Ratio

myUntil :: (a -> Bool) -> (a -> a) -> a -> a
myUntil p f x = head (dropWhile (not . p) (iterate f x))

egypt :: Rational -> [Rational]
egypt frac = reverse (snd (myUntil condicio step (frac, [])))
    where
        condicio (f, _) = f == 0

        step (f, acc) = (r, (1 % c) : acc)
            where
                x = numerator f

                y = denominator f

                (q, rMod) = (-y) `divMod` x

                c = -q

                r = rMod % (y * c)

main :: IO ()
main = do
    contingut <- getContents
    mapM_ processarLinia (lines contingut)
        where
            processarLinia linia = print (egypt (parse linia))

            parse :: String -> Rational
            parse linia = llegir (words string)
                where 
                    string = filter (`notElem` "()") linia

                    llegir [n, "%", d] = read n % read d