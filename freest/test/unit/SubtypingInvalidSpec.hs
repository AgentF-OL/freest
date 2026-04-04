module SubtypingInvalidSpec (spec) where

import Syntax.Module qualified as M
import UI.Error ( showErrors )
import Validation.Subtyping.Compare ( subtype )
import UnitSpecUtils ( mkComparisonSpec )

import Data.Map.Strict qualified as Map
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = mkComparisonSpec
  ["test/unit/SubtypingInvalid.test"]
  "Invalid subyping tests"
  \src (t, u, k, m) -> subtype m t u `shouldBe` False
