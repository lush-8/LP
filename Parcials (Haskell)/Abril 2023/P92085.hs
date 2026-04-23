
data Nat = Z | S Nat
    deriving Show


rec :: a -> (Nat -> a -> a) -> Nat -> a
rec base step Z = base 
rec base step (S n) = step n (rec base step n)


-- indica si un natural és parell o no
isEven :: Nat -> Bool 
isEven = rec base step
    where 
        base = True
        step _ prev = not prev 


-- retorna la suma de dos naturals
add :: Nat -> (Nat -> Nat)
add = rec base step 
    where 
        base y = y
        step _ prevAdd y = S (prevAdd y) 


-- retorna el producte de dos naturals
mul :: Nat -> (Nat -> Nat)
mul = rec base step 
    where 
        base _ = Z 
        step _ prevMul y = add y (prevMul y)


-- retorna el factorial d’un natural
fact :: Nat -> Nat 
fact = rec base step 
    where 
        base = S Z
        step n prevFact = mul (S n) prevFact