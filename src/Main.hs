module Main where

import Ideas.Common.Library
import Ideas.Main.Default

import Domain.Domain
import Domain.Exercises

main :: IO ()
main = defaultMain dr


dr :: DomainReasoner
dr = describe "Domain reasoner for tutorial" (newDomainReasoner "eval") 
    {
        exercises = [Some minimalExercise, Some basicExercise, Some evalExercise]
        , services  = myServices
    }

myServices :: [Service]
myServices = metaServiceList dr ++ serviceList


printAllDerivations :: IO ()
printAllDerivations = do
    let allExercises = zip (repeat evalExercise) [expr1, expr2, expr3, expr4, expr5, expr6, expr7, expr8]
    mapM_ (uncurry printDerivations) allExercises