module Data.Logic.Fml.Some (
  a,
  b,
  c,
  d,
  e,
  f,
  fml1,
  fml2,
  fml3,
) where

import qualified Data.Logic.Fml as Fml
import qualified Data.Logic.Var as Var

a = Fml.Final (Var.mk "a")

b = Fml.Final (Var.mk "b")

c = Fml.Final (Var.mk "c")

d = Fml.Final (Var.mk "d")

e = Fml.Final (Var.mk "e")

f = Fml.Final (Var.mk "f")

fml1 = Fml.And (Fml.And (Fml.Or (Fml.And (Fml.And a vz) vx) vy) b) (Fml.And c d)

fml2 = Fml.And a (Fml.And b (Fml.And c d))

fml3 = Fml.And (Fml.And (Fml.Or a b) (Fml.Or (Fml.Not b) c)) (Fml.Or (Fml.Or a b) (Fml.Not c))