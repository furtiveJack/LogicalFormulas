module Data.Logic.Fml.Combinator (
--  -- * Type
--
--  -- * Querying
  multOr
, multAnd
, allOf
, noneOf
, atLeast
, atLeastOne
, atMost
, atMostOne
, exactly
, exactlyOne
) where

import Data.List (subsequences)

import qualified Data.Logic.Fml      as Fml
import qualified Data.Logic.Var      as Var

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
allOf a = multAnd (allOf' a)
  where
    allOf' :: [Var.Var a] -> [Fml.Fml a]
    allOf' [x] = [Fml.Final x]
    allOf' (x:xs) = (Fml.Final x) : (allOf' xs)

-- |’noneOf’ @vs@ returns a formula that is satisfiable iff no variable
-- in @vs@ is true. The function returns @Nothing@ if @vs@ is the empty list.
noneOf :: [Var.Var a] -> Maybe (Fml.Fml a)
noneOf [] = Nothing
noneOf a = multAnd (noneOf' a)
  where
    noneOf' :: [Var.Var a] -> [Fml.Fml a]
    noneOf' [x] = [Fml.Not (Fml.Final x)]
    noneOf' (x:xs) = (Fml.Not (Fml.Final x)) : (noneOf' xs)

-- |’atLeast’ @vs@ @k@ returns a formula that is satisfied iff at least @k@
-- variables in @vs@ are true. The function returns @Nothing@ if @vs@ is the
-- empty list or @k@ is non-positive or @k@ is larger than the number of
-- variables in @vs@.
atLeast :: [Var.Var a] -> Int -> Maybe (Fml.Fml a)
atLeast [] k = Nothing
atLeast xs k
  | k <= 0 || k > (length xs) = Nothing
  | otherwise = Just (logicalCombinator (\var -> Fml.Final var) xs k)

-- |’atLeastOne’ @vs@ returns a formula that is satisfiable iff at least one
-- variable in @vs@ is true. The function returns @Nothing@ if @vs@ is the
-- empty list.
atLeastOne :: [Var.Var a] -> Maybe (Fml.Fml a)
atLeastOne lst = atLeast lst 1

-- |’atMost’ @vs@ @k@ returns a formula that is satisfiable iff at most @k@
-- variables in @vs@ are true. The function returns @Nothing@ if @vs@ is the
-- empty list or @k@ is non-positive or @k@ is larger than the number of
-- variables in @vs@.
atMost :: [Var.Var a] -> Int -> Maybe (Fml.Fml a)
atMost [] k = Nothing
atMost xs k
  | k <= 0 || k > (length xs) = Nothing
  | otherwise = Just (logicalCombinator (\var -> Fml.Not (Fml.Final var)) xs ((length xs) - k))

-- |’atMostOne’ @vs@ returns a formula that is satisfiable iff at most one
-- variable in @vs@ is true. The function returns @Nothing@ if @vs@ is the
-- empty list.
atMostOne :: [Var.Var a] -> Maybe (Fml.Fml a)
atMostOne lst = atMost lst 1

-- |’exactly’ @vs@ @k@ returns a formula that is satisfiable iff exactly @k@
-- variables in @vs@ are true. The function returns @Nothing@ if @vs@ is the
-- empty list or @k@ is non-positive or @k@ is larger than the number of
-- variables in @vs@.
exactly :: [Var.Var a] -> Int -> Maybe (Fml.Fml a)
exactly [] k = Nothing
exactly xs k
  | k <= 0 || k > (length xs) = Nothing
  | otherwise = Just (Fml.And (fromJust (atLeast xs k)) (fromJust (atMost xs ((length xs) - k))))

-- |’exactlyOne’ @vs@ returns a formula that is satisfiable iff exactly one
-- variable in @vs@ is true. The function returns @Nothing@ if @vs@ is the
-- empty list.
exactlyOne :: [Var.Var a] -> Maybe (Fml.Fml a)
exactlyOne lst = exactly lst 1

-- |`logicalCombinator` @lambda@ @xs@ @k@ returns a formula that is satisfiable if (depending on the lambda)
-- at most / at least @k@ variables in @vs@ are true. Parameters verifications must be done by the caller.
logicalCombinator :: (Var.Var a -> Fml.Fml a) -> [Var.Var a] -> Int ->  Fml.Fml a
logicalCombinator lambda xs k = applyOr (applyAnd lambda (getKCombi xs k))
  where
    getKCombi :: [Var.Var a] -> Int -> [[Var.Var a]]
    getKCombi lst k = filter (\variables -> (length variables) == k) (subsequences lst)

    applyAnd :: (Var.Var a -> Fml.Fml a) -> [[Var.Var a]] -> [Fml.Fml a]
    applyAnd lambda lst = map (\pList -> fromJust (multAnd (varLstToFmlLst lambda pList))) lst

    applyOr :: [Fml.Fml a] -> Fml.Fml a
    applyOr lst = fromJust (multOr lst)

    varLstToFmlLst :: (Var.Var a -> Fml.Fml a) -> [Var.Var a] -> [Fml.Fml a]
    varLstToFmlLst lambda lst = map (lambda) lst

-- |`fromJust` @a@ retrieves the value of @a@ if it is a @Just@, raises an error if it is a @Nothing@.
fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing = error "Nothing to get from this Maybe"