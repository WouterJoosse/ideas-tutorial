module Domain.Domain where

import Ideas.Common.Library

data Expr = 
    Div Expr Expr
    | Multiply Expr Expr 
    | Add Expr Expr 
    | Negate Expr 
    | Con Int
    deriving (Eq, Show, Read)

instance IsTerm Expr where
    toTerm (Div x y)      = binary divideSymbol (toTerm x) (toTerm y)
    toTerm (Multiply x y) = binary multiplySymbol (toTerm x) (toTerm y)
    toTerm (Add x y)      = binary addSymbol (toTerm x) (toTerm y)
    toTerm (Negate x)     = unary negateSymbol (toTerm x) 
    toTerm (Con x)        = TNum (toInteger x)

    fromTerm (TNum x) = return (Con (fromInteger  x))
    fromTerm term     = fromTermWith f term
        where 
            f s [x]   | s == negateSymbol   = return (Negate x)
            f s [x,y] | s == addSymbol      = return (Add x y)
                      | s == multiplySymbol = return (Multiply x y) 
                      | s == divideSymbol   = return (Div x y)
            f _ _ = fail "invalid expression" 


divideSymbol, multiplySymbol, addSymbol, negateSymbol :: Symbol
divideSymbol   = newSymbol "divide"
multiplySymbol = newSymbol "multiple"
addSymbol      = newSymbol "add"
negateSymbol   = newSymbol "negate"

eqExpr :: Expr -> Expr -> Bool
eqExpr x y = eval x == eval y

eval :: Expr -> Int
eval (Multiply x y) = eval x * eval y
eval (Add x y)      = eval x + eval y
eval (Negate x)     = -eval x
eval (Con x)        = x

isCon :: Expr -> Bool
isCon (Con _) = True
isCon _       = False

canSimplify :: Expr -> Bool
canSimplify (Div (Con x) (Con y)) = gcd x y > 1
canSimplify e = isCon e

-- expression 5 + (-2)
expr1 :: Expr
expr1 = Add (Con 5) (Negate (Con 2))

-- expression (-2) + (3 + 5)
expr2 :: Expr
expr2 = Add (Negate (Con 2)) (Add (Con 3) (Con 5))

-- expression (-2) * (3 + 5)
expr3 :: Expr
expr3 = Multiply (Negate (Con 2)) (Add (Con 3) (Con 5))

-- expression (1 - 2) * 3
expr4 :: Expr
expr4 = Multiply (Add (Con 1) (Negate (Con 2))) (Con 3)

-- expression (3/4) + (1/4)
expr5 :: Expr
expr5 = Add (Div (Con 3) (Con 4)) (Div (Con 1) (Con 4))

-- expression (4/5) * (2/5)
expr6 :: Expr
expr6 = Multiply (Div (Con 4) (Con 5)) (Div (Con 2) (Con 5))

-- expression (1/4) + (1/2 * 2)
expr7 :: Expr
expr7 = Add (Div (Con 1) (Con 4)) (Multiply (Div (Con 1) (Con 2)) (Con 2))

-- expression (2/4) * (1/2)
expr8 :: Expr
expr8 = Multiply (Div (Con 2) (Con 4)) (Div (Con 1) (Con 2))