import Data.List (elemIndex)

multEq :: Int -> Int -> [Int]
multEq x y = iterate (* (x * y)) 1

selectFirst :: [Int] -> [Int] -> [Int] -> [Int]
selectFirst l1 l2 l3 = filter check l1
    where
        check x = isLess (elemIndex x l2) (elemIndex x l3)

        isLess Nothing _ = False
        isLess (Just _) Nothing = True
        isLess (Just i) (Just j) = i < j

myIterate :: (a -> a) -> a -> [a]
myIterate f x = scanl step x (repeat ())
    where
        step acc _ = f acc

type SymTab a = String -> Maybe a

empty :: SymTab a
empty _ = Nothing

get :: SymTab a -> String -> Maybe a
get st key = st key

set :: SymTab a -> String -> a -> SymTab a
set st key val searchKey
    | searchKey == key = Just val
    | otherwise = st searchKey

data Expr a
    = Val a
    | Var String
    | Sum (Expr a) (Expr a)
    | Sub (Expr a) (Expr a)
    | Mul (Expr a) (Expr a)
    deriving Show

eval :: (Num a) => SymTab a -> Expr a -> Maybe a
eval _ (Val v) = Just v
eval st (Var s) = get st s
eval st (Sum e1 e2) = do
    v1 <- eval st e1
    v2 <- eval st e2
    return (v1 + v2)
eval st (Sub e1 e2) = do
    v1 <- eval st e1
    v2 <- eval st e2
    return (v1 - v2)
eval st (Mul e1 e2) = do
    v1 <- eval st e1
    v2 <- eval st e2
    return (v1 * v2)