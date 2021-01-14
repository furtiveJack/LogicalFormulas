import Test.HUnit
import Test.Hspec
import Test.QuickCheck

import qualified Data.Logic.Fml as Fml
import qualified Data.Logic.Fml.Combinator as Combinator
import qualified Data.Logic.Fml.Some as Some
import qualified Data.Logic.Var as Var
import qualified Data.Logic.Fml.Some as Some

main :: IO ()
main = hspec $ do
  describe "Data.Logic.Fml.Combinator unit tests" $ do
    it "should be equals to Nothing" $ do
      fmap Fml.prettyFormat (Combinator.atLeast [Var.mk i | i <- [1..4]] 0) == Nothing

    it "should be equals to allOfFml" $ do
      (fmap Fml.prettyFormat $ Combinator.allOf [Var.mk i | i <- [1..4]]) == (fmap Fml.prettyFormat $ (Just (Some.allOfFml)))

    it "should be equals to noneOfFml" $ do
      (fmap Fml.prettyFormat $ Combinator.noneOf [Var.mk i | i <- [1..4]]) == (fmap Fml.prettyFormat $ (Just (Some.noneOfFml)))

    it "should be equals to Nothing" $ do
      (fmap Fml.prettyFormat $ Combinator.atLeast [Var.mk i | i <- [1..4]] 0) == Nothing

    it "should be equals to atLeastOneFml" $ do
      (fmap Fml.prettyFormat $ Combinator.atLeast [Var.mk i | i <- [1..4]] 1) == (fmap Fml.prettyFormat $ (Just (Some.atLeastOneFml)))

    it "should be equals to atLeastTwoFml" $ do
      (fmap Fml.prettyFormat $ Combinator.atLeast [Var.mk i | i <- [1..4]] 2) == (fmap Fml.prettyFormat $ (Just (Some.atLeastTwoFml)))

    it "should be equals to atLeastOneFml" $ do
      (fmap Fml.prettyFormat $ Combinator.atLeastOne [Var.mk i | i <- [1..4]]) == (fmap Fml.prettyFormat $ (Just (Some.atLeastOneFml)))

    it "should be equals to Nothing" $ do
      (fmap Fml.prettyFormat $ Combinator.atMost [Var.mk i | i <- [1..4]] 0) == Nothing

    it "should be equals to atMostOneFml" $ do
      (fmap Fml.prettyFormat $ Combinator.atMost [Var.mk i | i <- [1..4]] 1) == (fmap Fml.prettyFormat $ (Just (Some.atMostOneFml)))

    it "should be equals to atMostTwoFml" $ do
      (fmap Fml.prettyFormat $ Combinator.atMost [Var.mk i | i <- [1..4]] 2) == (fmap Fml.prettyFormat $ (Just (Some.atMostTwoFml)))

    it "should be equals to atMostOneFml" $ do
      (fmap Fml.prettyFormat $ Combinator.atMostOne [Var.mk i | i <- [1..4]]) == (fmap Fml.prettyFormat $ (Just (Some.atMostOneFml)))

    it "should be equals to Nothing" $ do
      (fmap Fml.prettyFormat $ Combinator.exactly [Var.mk i | i <- [1..4]] 0) == Nothing

    it "should be equals to exactlyOneFml" $ do
      (fmap Fml.prettyFormat $ Combinator.exactly [Var.mk i | i <- [1..4]] 1) == (fmap Fml.prettyFormat $ (Just (Some.exactlyOneFml)))

    it "should be equals to exactlyTwoFml" $ do
      (fmap Fml.prettyFormat $ Combinator.exactly [Var.mk i | i <- [1..4]] 2) == (fmap Fml.prettyFormat $ (Just (Some.exactlyTwoFml)))

    it "should be equals to exactlyOneFml" $ do
      (fmap Fml.prettyFormat $ Combinator.exactlyOne [Var.mk i | i <- [1..4]]) == (fmap Fml.prettyFormat $ (Just (Some.exactlyOneFml)))
