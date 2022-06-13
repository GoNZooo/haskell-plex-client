module ExampleSpec (spec) where

import Qtility
import Types
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "`True` is `True`" $ do
    it "is so" $ do
      True `shouldBe` True
