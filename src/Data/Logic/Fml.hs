module Data.Logic.Fml (
--  -- * Type
  Fml (..)
--
--  -- * Querying
, depth
, vars
--
--  -- * Formatting
, prettyFormat
--
--  -- * Transforming
, toNNF
, toCNF
--, toCCNF
, toDNF
, toUniversalNAnd
, toUniversalNOr
--
--  -- * Testing
, isNNF
, isCNF
--, isCCNF
, isDNF
, isUniversalNAnd
, isUniversalNOr
) where

import qualified Data.Foldable  as F
import qualified Data.List      as L

import           Data.Logic.Var as Var

data Fml a = And   (Fml a) (Fml a)
           | NAnd  (Fml a) (Fml a)
           | Or    (Fml a) (Fml a)
           | NOr   (Fml a) (Fml a)
           | XOr   (Fml a) (Fml a)
           | XNOr  (Fml a) (Fml a)
           | Imply (Fml a) (Fml a)
           | Equiv (Fml a) (Fml a)
           | Not   (Fml a)
           | Final (Var.Var a)
           deriving (Show)

prettyFormat :: (Show a) => Fml a -> String
prettyFormat (And   p q) = "(" ++ prettyFormat p ++ " . "   ++ prettyFormat q ++ ")"
prettyFormat (NAnd  p q) = "(" ++ prettyFormat p ++ " ~. "  ++ prettyFormat q ++ ")"
prettyFormat (Or    p q) = "(" ++ prettyFormat p ++ " + "   ++ prettyFormat q ++ ")"
prettyFormat (NOr   p q) = "(" ++ prettyFormat p ++ " ~+ "  ++ prettyFormat q ++ ")"
prettyFormat (XOr   p q) = "(" ++ prettyFormat p ++ " x+ "  ++ prettyFormat q ++ ")"
prettyFormat (XNOr  p q) = "(" ++ prettyFormat p ++ " x~+ " ++ prettyFormat q ++ ")"
prettyFormat (Imply p q) = "(" ++ prettyFormat p ++ " => "  ++ prettyFormat q ++ ")"
prettyFormat (Equiv p q) = "(" ++ prettyFormat p ++ " <=> " ++ prettyFormat q ++ ")"
prettyFormat (Not   p)   = "-" ++ prettyFormat p
prettyFormat (Final v)   = show v

-- |’vars’ @p@ returns all variables that occur in formula @p@. Duplicate
-- occurrences are removes.
vars :: (Eq a) => Fml a -> [Var.Var a]
vars a = L.nub (varsTmp a)

varsTmp :: (Eq a) => Fml a -> [Var.Var a]
varsTmp (And p q) = (vars p) ++ (vars q)
varsTmp (NAnd p q) = (vars p) ++ (vars q)
varsTmp (Or p q) = (vars p) ++ (vars q)
varsTmp (NOr p q) = (vars p) ++ (vars q)
varsTmp (XOr p q) = (vars p) ++ (vars q)
varsTmp (XNOr p q) = (vars p) ++ (vars q)
varsTmp (Imply p q) = (vars p) ++ (vars q)
varsTmp (Equiv p q) = (vars p) ++ (vars q)
varsTmp (Not p) = vars p
varsTmp (Final v) = [v]

-- |’depth’ @p@ return the depth of fomula @p@.
depth :: (Num b, Ord b) => Fml a -> b
depth (And p q) = 1 + max (depth p) (depth q)
depth (NAnd p q) = 1 + max (depth p) (depth q)
depth (Or p q) = 1 + max (depth p) (depth q)
depth (NOr p q) = 1 + max (depth p) (depth q)
depth (XOr p q) = 1 + max (depth p) (depth q)
depth (XNOr p q) = 1 + max (depth p) (depth q)
depth (Imply p q) = 1 + max (depth p) (depth q)
depth (Equiv p q) = 1 + max (depth p) (depth q)
depth (Not p) = 1 + depth p
depth (Final v) = 0

-- THERE ARE PROBLEMS BRO
-- |’toNNF’ @f@ converts the formula @f@ to NNF.
toNNF :: Fml a -> Fml a
toNNF (Final a) = Final a
toNNF (Not (Not a)) = toNNF a
toNNF (And p q) = And (toNNF p) (toNNF q)
toNNF (Not (And p q)) = toNNF (NAnd p q)
toNNF (NAnd p q) = Or (Not (toNNF p)) (Not (toNNF q))
toNNF (Or p q) = Or (toNNF p) (toNNF q)
toNNF (Not (Or p q)) = toNNF (NOr p q)
toNNF (NOr p q) = And (Not (toNNF p)) (Not (toNNF q))
toNNF (XOr p q) = And (Or (toNNF p) (toNNF q)) (Not (And (toNNF p) (toNNF q)))
toNNF (Not (XOr p q)) = toNNF (XNOr p q)
toNNF (XNOr p q) = Or (And (toNNF p) (toNNF q)) (And (Not (toNNF p)) (Not (toNNF q)))
toNNF (Imply p q) = Or (toNNF q) (Not (toNNF p))
toNNF (Equiv p q) = And (toNNF (Imply (toNNF p) (toNNF q))) (toNNF (Imply (toNNF q) (toNNF p)))
toNNF (Not p) = Not (toNNF p)

-- |’toCNF’ @f@ converts the formula @f@ to CNF.
toCNF :: Fml a -> Fml a
toCNF = toCNF' . toNNF
  where
    toCNF' :: Fml a -> Fml a
    toCNF' (Final a) = Final a
    toCNF' (And p q) = And (toCNF' p) (toCNF' q)
    toCNF' (Or p q) = (toCNF' p) `distribution` (toCNF' q)
    toCNF' (Not p) = Not p

    distribution :: Fml a -> Fml a -> Fml a
    distribution (And p q) r = And (p `distribution` r) (q `distribution` r)
    distribution p (And q r) = And (p `distribution` q) (p `distribution` r)
    distribution p q = Or p q

-- |’toDNF’ @f@ converts the formula @f@ to DNF.
toDNF :: Fml a -> Fml a
toDNF = toDNF' . toNNF
  where
    toDNF' :: Fml a -> Fml a
    toDNF' (Final a) = Final a
    toDNF' (And p q) = (toDNF' p) `distribution` (toDNF' q)
    toDNF' (Or p q) = Or (toDNF' p) (toDNF' q)
    toDNF' (Not p) = Not p

    distribution :: Fml a -> Fml a -> Fml a
    distribution (Or p q) r = Or (p `distribution` r) (q `distribution` r)
    distribution p (Or q r) = Or (p `distribution` q) (p `distribution` r)
    distribution p q = And p q

-- |’isNNF’ @f@ returns true iff formula @f@ is NNF.
isNNF :: Fml a -> Bool
isNNF (Final a) = True
isNNF (Not (Final a)) = True
isNNF (And p q) = (isNNF p) && (isNNF q)
isNNF (Or p q) = (isNNF p) && (isNNF q)
isNNF _ = False

-- |’isCNF’ @f@ returns true iff formula @f@ is CNF.
isCNF :: Fml a -> Bool
isCNF (Or p (And q r)) = False
isCNF (Or (And p q) r) = False
isCNF (And p q) = (isCNF p) && (isCNF q)
isCNF (Or p q) = (isCNF p) && (isCNF q)
isCNF a = isNNF a

-- |’isDNF’ @f@ returns true iff formula @f@ is DNF.
isDNF :: Fml a -> Bool
isDNF (And p (Or q r)) = False
isDNF (And (Or p q) r) = False
isDNF (Or p q) = (isDNF p) && (isDNF q)
isDNF (And p q) = (isDNF p) && (isDNF q)
isDNF a = isNNF a

-- |’toUniversalNAnd’ @p@ returns a NAND-formula that is equivalent
-- to formula @p@.
toUniversalNAnd :: Fml a -> Fml a
toUniversalNAnd = toUniversalNAnd' . toNNF
  where
    toUniversalNAnd' :: Fml a -> Fml a
    toUniversalNAnd' (Final a) = Final a
    toUniversalNAnd' (Not p) = NAnd (toUniversalNAnd' p) (toUniversalNAnd' p)
    toUniversalNAnd' (Or p q) = NAnd (NAnd (toUniversalNAnd' p) (toUniversalNAnd' p)) (NAnd (toUniversalNAnd' q) (toUniversalNAnd' q))
    toUniversalNAnd' (And p q) = NAnd (NAnd (toUniversalNAnd' p) (toUniversalNAnd' q)) (NAnd (toUniversalNAnd' p) (toUniversalNAnd' q))

-- |’toUniversalNOr’ @p@ returns a NOR-formula that is equivalent
-- to formula @p@.
toUniversalNOr :: Fml a -> Fml a
toUniversalNOr = toUniversalNOr' . toNNF
  where
    toUniversalNOr' :: Fml a -> Fml a
    toUniversalNOr' (Final a) = Final a
    toUniversalNOr' (Not p) = NOr (toUniversalNOr' p) (toUniversalNOr' p)
    toUniversalNOr' (Or p q) = NOr (NOr (toUniversalNOr' p) (toUniversalNOr' q)) (NOr (toUniversalNOr' p) (toUniversalNOr' q))
    toUniversalNOr' (And p q) = NOr (NOr (toUniversalNOr' p) (toUniversalNOr' p)) (NOr (toUniversalNOr' q) (toUniversalNOr' q))

-- |’isUniversalNAnd’ @p@ returns true iff formula @p@ uses only NAND
-- and variables.
isUniversalNAnd :: Fml a -> Bool
isUniversalNAnd (Final a) = True
isUniversalNAnd (NAnd p q) = (isUniversalNAnd p) && (isUniversalNAnd q)
isUniversalNAnd _ = False

-- |’isUniversalNOr’ @p@ returns true iff formula @p@ uses only NOR
-- and variables.
isUniversalNOr :: Fml a -> Bool
isUniversalNOr (Final a) = True
isUniversalNOr (NOr p q) = (isUniversalNOr p) && (isUniversalNOr q)
isUniversalNOr _ = False

-- |’toCCNF’ @f@ converts the formula @f@ to CCNF.
toCCNF :: Fml a -> Fml a

-- |’isCCNF’ @f@ returns true iff formula @f@ is CCNF.
--isCCNF :: Fml a -> Bool

