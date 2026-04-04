module SubtypingValidSpec (spec) where

import Syntax.Module qualified as M
import UI.Error ( showErrors )
import Validation.Subtyping ( subtypeOf )
import UnitSpecUtils ( mkSubtypingSpec )

import Data.Map.Strict qualified as Map
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = mkSubtypingSpec
  ["test/unit/SubtypingValid.test"]
  "Valid subyping tests"
  \src (t, u, k, m) -> subtypeOf m t u `shouldBe` True
