fizzBuzz :: [Either Int String]
fizzBuzz = map filter [0..]
    where
        filter n
            | n `mod` 3 == 0 && n `mod` 5 == 0 = Right "FizzBuzz"
            | n `mod` 3 == 0 = Right "Fizz"
            | n `mod` 5 == 0 = Right "Buzz"
            | otherwise = Left n