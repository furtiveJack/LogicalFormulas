module Data.Logic.Fml.Combinator (
--  -- * Type
--
--  -- * Querying
  multOr
, multAnd
, allOf
--, noneOf
--, atLeast
--, atLeastOne
--, atMost
--, atMostOne
--, exactly
--, exactlyOne
) where

import qualified Data.Logic.Fml      as Fml

-- |’multOr’ @fs@ returns the disjunction of the formulas in @fs.
-- It returns @Nothing@ if @fs@ is the empty list.
--
-- >>> Combinator.multOr []
-- Nothing
-- >>> multOr [Fml.Final (Var.mk i) | i <- [1..4]]
-- Just (Or (Final 1) (Or (Final 2) (Or (Final 3) (Final 4))))
multOr :: [Fml.Fml a] -> Maybe (Fml.Fml a)
multOr [] = Nothing
multOr x = Just (multOr' x)
  where
    multOr' :: [Fml.Fml a] -> Fml.Fml a
    multOr' (x:[]) = x
    multOr' (x:xs) = (Fml.Or x (multOr' xs))

-- |’multAnd’ @fs@ returns the conjunction of the formulas in @fs.
-- It returns @Nothing@ if @fs@ is the empty list.
--
-- >>> Combinator.multAnd []
-- Nothing
-- multAnd [Fml.Final (Var.mk i) | i <- [1..4]]
-- Just (And (Final 1) (And (Final 2) (And (Final 3) (Final 4))))
multAnd :: [Fml.Fml a] -> Maybe (Fml.Fml a)
multAnd [] = Nothing
multAnd x = Just (multAnd' x)
  where
    multAnd' :: [Fml.Fml a] -> Fml.Fml a
    multAnd' (x:[]) = x
    multAnd' (x:xs) = (Fml.And x (multAnd' xs))

-- |’allOf’ @vs@ returns a formula that is satisfiable iff all variables
-- in @vs@ are true. The function returns @Nothing@ if @vs@ is the empty list.
allOf :: [Var.Var a] -> Maybe (Fml.Fml a)
allOf [] = Nothing


-- |’noneOf’ @vs@ returns a formula that is satisfiable iff no variable
-- in @vs@ is true. The function returns @Nothing@ if @vs@ is the empty list.
--noneOf :: [Var.Var a] -> Maybe (Fml.Fml a)

-- |’atLeast’ @vs@ @k@ returns a formula that is satisfied iff at least @k@
-- variables in @vs@ are true. The function returns @Nothing@ if @vs@ is the
-- empty list or @k@ is non-positive or @k@ is larger than the number of
-- variables in @vs@.
atLeast :: [Var.Var a] -> Int -> Maybe (Fml.Fml a)
atLeast [] = Nothing
atLeast (a, b) = if b <= 0 || b > (length a) then Nothing else

-- |’atLeastOne’ @vs@ returns a formula that is satisfiable iff at least one
-- variable in @vs@ is true. The function returns @Nothing@ if @vs@ is the
-- empty list.
--atLeastOne :: [Var.Var a] -> Maybe (Fml.Fml a)

-- |’atMost’ @vs@ @k@ returns a formula that is satisfiable iff at most @k@
-- variables in @vs@ are true. The function returns @Nothing@ if @vs@ is the
-- empty list or @k@ is non-positive or @k@ is larger than the number of
-- variables in @vs@.
--atMost :: [Var.Var a] -> Int -> Maybe (Fml.Fml a)

-- |’atMostOne’ @vs@ returns a formula that is satisfiable iff at most one
-- variable in @vs@ is true. The function returns @Nothing@ if @vs@ is the
-- empty list.
--atMostOne :: [Var.Var a] -> Maybe (Fml.Fml a)

-- |’exactly’ @vs@ @k@ returns a formula that is satisfiable iff exactly @k@
-- variables in @vs@ are true. The function returns @Nothing@ if @vs@ is the
-- empty list or @k@ is non-positive or @k@ is larger than the number of
-- variables in @vs@.
--exactly :: [Var.Var a] -> Int -> Maybe (Fml.Fml a)

-- |’exactlyOne’ @vs@ returns a formula that is satisfiable iff exactly one
-- variable in @vs@ is true. The function returns @Nothing@ if @vs@ is the
-- empty list.
--exactlyOne :: [Var.Var a] -> Maybe (Fml.Fml a)

