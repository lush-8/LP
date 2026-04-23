data LogicExpression
        = Or  LogicExpression LogicExpression
        | And LogicExpression LogicExpression
        | Not LogicExpression
        | Var String
        | Val Bool

instance Show LogicExpression where
    show (Val True) = "1"
    show (Val False) = "0"
    show (Var s) = s
    show (Not e) = "(not " ++ show e ++ ")"
    show (And e1 e2) = "(" ++ show e1 ++ " and " ++ show e2 ++ ")" 
    show (Or e1 e2) = "(" ++ show e1 ++ " or " ++ show e2 ++ ")"

pushNegations :: LogicExpression -> LogicExpression
pushNegations (Not (Or e1 e2)) = And (pushNegations (Not e1)) (pushNegations (Not e2))
pushNegations (Not (And e1 e2)) = Or (pushNegations (Not e1)) (pushNegations (Not e2))
pushNegations (Not (Not e)) = pushNegations e
pushNegations (Not (Var s)) = Not (Var s)
pushNegations (Not (Val b)) = Val (not b)
pushNegations (Or e1 e2) = Or (pushNegations e1) (pushNegations e2)
pushNegations (And e1 e2) = And (pushNegations e1) (pushNegations e2)
pushNegations element = element

bits :: [[[Int]]]
bits = iterate ((:) <$> [0, 1] <*>) [[]]