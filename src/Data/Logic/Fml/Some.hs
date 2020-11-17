module Data.Logic.Fml.Some (
  fml1,
  fml2,
  fml3,
--  fml4,
--  fml5,
--  fml6,
--  fml7,
--  fml8,
--  fml9,
--  fml10,
--  fml11
) where

import qualified Data.Logic.Fml as Fml
import qualified Data.Logic.Var as Var


fml1 = Fml.Final (Var.mk "a")

fml2 = Fml.Final (Var.mk "1")

fml3 = Fml.And fml1 fml2