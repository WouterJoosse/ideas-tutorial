module Domain.Exercises (
    minimalExercise
    , basicExercise
    , evalExercise
) where

import Ideas.Common.Library

import Domain.Domain
import Domain.Strategies

minimalExercise :: Exercise Expr
minimalExercise = emptyExercise
    {
        exerciseId = describe "Evaluate an expression (minimal)" $ newId "eval.minimal"
        , strategy = liftToContext addOrNegate
        , prettyPrinter = show
    }

basicExercise :: Exercise  Expr
basicExercise = emptyExercise
    {
        exerciseId      = describe "Evaluate an expression (basic)" $ newId "eval.basic"
        , strategy      = evalStrategy
        , navigation    = termNavigator
        , prettyPrinter = show
    }

evalExercise :: Exercise  Expr
evalExercise = emptyExercise
    {
        exerciseId      = describe "Evaluate an expression (full)" $ newId "eval.full"
        , status        = Experimental
        , strategy      = evalStrategy
        , prettyPrinter = show
        , navigation    = termNavigator
        , parser        = readM
        , equivalence   = withoutContext eqExpr
        , similarity    = withoutContext (==)
        , ready         = predicate . ors $ [not . canSimplify, isCon]
        , examples      = examplesFor Easy [expr1, expr2]
    }
