module Domain.Rules (
    addRule
    , negateRule
    , multiplyRule
    , combineSameDivisorRule
    , simplifyDivToIntegerRule
    , distributionRule
    , rewriteMixedFracionRule
    , divDivRule
    , simplifyDivisionWithGCDRule
)where

import Domain.Domain

import Ideas.Common.Library

addRule :: Rule Expr
addRule = describe "Add two numbers" $ makeRule "eval.add" f
    where 
        f :: Expr -> Maybe Expr
        f (Add (Con x) (Con y)) = Just $ Con (x + y)
        f _ = Nothing

negateRule :: Rule Expr
negateRule = describe "Negate number" $ makeRule "eval.negate" f 
    where 
        f :: Expr -> Maybe Expr
        f (Negate (Con x)) = Just $ Con (-x)
        f _ = Nothing

multiplyRule :: Rule Expr
multiplyRule = describe "Multiply two numbers" $ makeRule "eval.multiply" f
    where 
        f :: Expr -> Maybe Expr
        f (Multiply (Con x) (Con y)) = Just $ Con (x * y)
        f (Multiply (Div x y) z) = Just $ Div (Multiply x z) y
        f (Multiply z (Div x y)) = Just $ Div (Multiply x z) y
        f _ = Nothing

combineSameDivisorRule :: Rule Expr
combineSameDivisorRule = describe "Simplify divisions with same divisor" $ makeRule "eval.div.simplifySameDivisor" f
    where 
        f :: Expr -> Maybe Expr
        f (Add (Div x z) (Div y z'))      = if z == z' then Just $ Div (Add x y) z else Nothing 
        f (Multiply (Div x z) (Div y z')) = if z == z' then Just $ Div (Multiply x y) (Multiply z z') else Nothing
        f _ = Nothing 

simplifyDivToIntegerRule :: Rule Expr
simplifyDivToIntegerRule = describe "Simplify a division to an integer" $ makeRule "eval.div.divtointeger" f
    where 
        f :: Expr -> Maybe Expr
        f (Div x (Con 0)) = error "You can not divide by zero!"
        f (Div x y) = if x == y then Just $ Con 1 else Nothing
        f _ = Nothing

simplifyDivisionWithGCDRule :: Rule Expr
simplifyDivisionWithGCDRule = describe "Simplify a division with GCD" $ makeRule "eval.div.simplifyWithGCD" f
    where 
        f :: Expr -> Maybe Expr
        f (Div (Con x) (Con y)) = 
            let 
                d = gcd x y 
            in 
                if d > 1 then Just $ Div (Con (x `div` d)) (Con (y `div` d)) else Nothing
        f _ = Nothing


divDivRule :: Rule Expr
divDivRule = describe "Simplify division of division" $ makeRule "eval.div.divdiv" f
    where
        f :: Expr -> Maybe Expr
        f (Div (Div x y) z) = Just $ Div x (Multiply y z)
        f _ = Nothing

rewriteMixedFracionRule :: Rule Expr
rewriteMixedFracionRule = describe "Rewrite a mixed fraction: 1/4 + 1 --> 1 + 1/4" $ makeRule "eval.div.rewriteMixed" f
    where
        f :: Expr -> Maybe Expr
        f (Add (Div x y) (Con z)) = Just $ Add (Con z) (Div x y)
        f _ = Nothing

distributionRule :: Rule Expr
distributionRule = describe "Distribute multiplication" $ makeRule "eval.distribute" f
    where 
        f :: Expr -> Maybe Expr
        f (Multiply x (Add y z)) = Just $ Add (Multiply x y) (Multiply x z)
        f (Multiply (Add x y) z) = Just $ Add (Multiply x z) (Multiply y z)
        f _  = Nothing
