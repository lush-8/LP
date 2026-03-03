myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f v [] = v
myFoldl f v (x:xs) = myFoldl f (f v x) xs

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f v [] = v
myFoldr f v (x:xs) = f x (myFoldr f v xs)

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

myUntil :: (a -> Bool) -> (a -> a) -> a -> a
myUntil p f x
    | p x = x
    | otherwise = myUntil p f (f x)

myMap :: (a -> b) -> [a] -> [b]
myMap f xs = myFoldr (\x acc -> (f x) : acc) [] xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p xs = myFoldr (\x acc -> if (p x) then (x : acc) else acc) [] xs

myAll :: (a -> Bool) -> [a] -> Bool
myAll p xs = myFoldr (\x acc -> (p x) && acc) True xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny p xs = myFoldr (\x acc -> (p x) || acc) False xs

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f xs ys = myMap (\(a, b) -> f a b) (myZip xs ys)