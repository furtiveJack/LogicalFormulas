module Data.Logic.Fml.Some (
  vx,
  vy,
  vz,
  fml1,
  fml2,
  fml3,
  fml4,
  fml5,
  fml6,
  fml7
) where

import qualified Data.Logic.Fml as Fml
import qualified Data.Logic.Var as Var


vx = Fml.Final (Var.mk "x")

vy = Fml.Final (Var.mk "y")

vz = Fml.Final (Var.mk "z")

fml1 = Fml.Not vx

fml2 = Fml.And vx vy

fml3 = Fml.Imply vx vy

fml4 = Fml.Equiv vx vy

fml5 = Fml.XOr vx vy

fml6 = Fml.XNOr vx vy

fml7 = Fml.NOr vx vy