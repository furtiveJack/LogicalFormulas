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
--, toNNF
--, toCNF
--, toCCNF
--, toDNF
--, toUniversalNAnd
--
--  -- * Testing
--, isNNF
--, isCNF
--, isCCNF
--, isDNF
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

-- |’toNNF’ @f@ converts the formula @f@ to NNF.
toNNF :: Fml a -> Fml a


-- |’toCNF’ @f@ converts the formula @f@ to CNF.
--toCNF :: Fml a -> Fml a

-- |’toDNF’ @f@ converts the formula @f@ to DNF.
--toDNF :: Fml a -> Fml a

-- |’isNNF’ @f@ returns true iff formula @f@ is NNF.
--isNNF :: Fml a -> Fml a

-- |’isCNF’ @f@ returns true iff formula @f@ is CNF.
--isCNF :: Fml a -> Fml a

-- |’isDNF’ @f@ returns true iff formula @f@ is DNF.
--isDNF :: Fml a -> Fml a

-- |’toUniversalNAnd’ @p@ returns a NAND-formula that is equivalent
-- to formula @p@.
--toUniversalNAnd :: Fml a -> Fml a

-- |’toUniversalNOr’ @p@ returns a NOR-formula that is equivalent
-- to formula @p@.
--toUniversalNOr :: Fml a -> Fml a

-- |’isUniversalNAnd’ @p@ returns true iff formula @p@ uses only NAND
-- and variables.
--isUniversalNAnd :: Fml a -> Bool

-- |’isUniversalNOr’ @p@ returns true iff formula @p@ uses only NOR
-- and variables.
--isUniversalNOr :: Fml a -> Bool

-- |’toCCNF’ @f@ converts the formula @f@ to CCNF.
--toCCNF :: Fml a -> Fml a

-- |’isCCNF’ @f@ returns true iff formula @f@ is CCNF.
--isCCNF :: Fml a -> Bool

