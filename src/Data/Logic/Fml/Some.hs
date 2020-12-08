module Data.Logic.Fml.Some (
  vx,
  vy,
  vz,
  fml1,
  fml2,
  fml3,
  fml4
) where

import qualified Data.Logic.Fml as Fml
import qualified Data.Logic.Var as Var


vx = Fml.Final (Var.mk "x")

vy = Fml.Final (Var.mk "y")

vz = Fml.Final (Var.mk "z")

fml1 = Fml.Not vx

fml2 = Fml.And vx vy

fml3 = Fml.Or vy (Fml.Not vz)

fml4 = Fml.Equiv (Fml.And vx vy) (Fml.Or vy (Fml.Not vz))