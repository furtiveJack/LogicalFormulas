module Data.Logic.Fml.Some (
  vx,
  vy,
  vz,
  a,
  b,
  c,
  d,
  fml1,
  fml2,
  fml3,
  fml4,
  fml5,
  fml6,
  fml7,
  fml8,
  fml9,
  fml10
) where

import qualified Data.Logic.Fml as Fml
import qualified Data.Logic.Var as Var


vx = Fml.Final (Var.mk "x")

vy = Fml.Final (Var.mk "y")

vz = Fml.Final (Var.mk "z")

a = Fml.Final (Var.mk "a")

b = Fml.Final (Var.mk "b")

c = Fml.Final (Var.mk "c")

d = Fml.Final (Var.mk "d")

fml1 = Fml.Not vx

fml2 = Fml.And vx vy

fml3 = Fml.Imply vx vy

fml4 = Fml.Equiv vx vy

fml5 = Fml.XOr vx vy

fml6 = Fml.XNOr vx vy

fml7 = Fml.Not (Fml.And vx vz)

fml8 = Fml.NAnd a b

fml9 = Fml.NAnd (Fml.NAnd a b) c

fml10 = Fml.NAnd (Fml.And a b) c