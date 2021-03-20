module Domain.Strategies
  ( addOrNegate
  , evalStrategy
  ) where

import           Ideas.Common.Library

import           Domain.Domain
import           Domain.Rules

addOrNegate :: LabeledStrategy Expr
addOrNegate = label "add-or-negate" $ addRule .|. negateRule

multiply :: LabeledStrategy Expr
multiply = label "multiply" multiplyRule

distribute :: LabeledStrategy Expr
distribute = label "distribute" distributionRule

combineSameDivisor :: LabeledStrategy Expr
combineSameDivisor = label "combineSameDivisor" combineSameDivisorRule

simplifyDivToInteger :: LabeledStrategy Expr
simplifyDivToInteger = label "simplifyDivToInteger" simplifyDivToIntegerRule

rewriteMixedFracion :: LabeledStrategy Expr
rewriteMixedFracion = label "rewriteMixedFraction" rewriteMixedFracionRule

divDiv :: LabeledStrategy Expr
divDiv = label "simplifyDivisionOfDivision" divDivRule

simplifyDivisionWithGCD :: LabeledStrategy Expr
simplifyDivisionWithGCD =
  label "simplifyDivisionWithGCD" simplifyDivisionWithGCDRule

evalStrategy :: LabeledStrategy (Context Expr)
evalStrategy = label "eval" . repeatS . somewhere $ liftToContext
  (   addOrNegate
  .|. multiply
  .|. distribute
  .|. combineSameDivisor
  .|. simplifyDivToInteger
  .|. rewriteMixedFracion
  .|. divDiv
  .|. simplifyDivisionWithGCD
  )
