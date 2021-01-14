module Data.Logic.Fml.Some (
  v1,
  v2,
  v3,
  v4,
  v5,
  v6,
  allOfFml,
  noneOfFml,
  atLeastOneFml,
  atLeastTwoFml,
  atMostOneFml,
  atMostTwoFml,
  exactlyOneFml,
  exactlyTwoFml
) where

import qualified Data.Logic.Fml as Fml
import qualified Data.Logic.Var as Var

v1 = Fml.Final (Var.mk 1)

v2 = Fml.Final (Var.mk 2)

v3 = Fml.Final (Var.mk 3)

v4 = Fml.Final (Var.mk 4)

v5 = Fml.Final (Var.mk 5)

v6 = Fml.Final (Var.mk 6)

allOfFml = Fml.And v1 (Fml.And v2 (Fml.And v3 v4))

noneOfFml = Fml.And (Fml.Not v1) (Fml.And (Fml.Not v2) (Fml.And (Fml.Not v3) (Fml.Not v4)))

atLeastOneFml = Fml.Or v1 (Fml.Or v2 (Fml.Or v3 v4))

atLeastTwoFml = Fml.Or (Fml.And v1 v2) (Fml.Or (Fml.And v1 v3) (Fml.Or (Fml.And v2 v3) (Fml.Or (Fml.And v1 v4) (Fml.Or (Fml.And v2 v4) (Fml.And v3 v4)))))

atMostOneFml = Fml.Or (Fml.And (Fml.Not v1) (Fml.And (Fml.Not v2) (Fml.Not v3)))
                      (Fml.Or (Fml.And (Fml.Not v1) (Fml.And (Fml.Not v2) (Fml.Not v4)))
                              (Fml.Or (Fml.And (Fml.Not v1) (Fml.And (Fml.Not v3) (Fml.Not v4)))
                                      (Fml.And (Fml.Not v2) (Fml.And (Fml.Not v3) (Fml.Not v4)))))

atMostTwoFml = Fml.Or (Fml.And (Fml.Not v1) (Fml.Not v2))
                      (Fml.Or (Fml.And (Fml.Not v1) (Fml.Not v3))
                              (Fml.Or (Fml.And (Fml.Not v2) (Fml.Not v3))
                                      (Fml.Or (Fml.And (Fml.Not v1) (Fml.Not v4))
                                              (Fml.Or (Fml.And (Fml.Not v2) (Fml.Not v4))
                                                      (Fml.And (Fml.Not v3) (Fml.Not v4))))))

exactlyOneFml = Fml.And atLeastOneFml atMostOneFml

exactlyTwoFml = Fml.And atLeastTwoFml atMostTwoFml