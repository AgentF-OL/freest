
module EquivalenceInvalidSpec (spec) where

import Syntax.Module qualified as M
import UI.Error ( showErrors )
import Validation.Subtyping.Compare ( equivalent )
import UnitSpecUtils ( mkComparisonSpec )

import Data.Map.Strict qualified as Map
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = mkComparisonSpec
  ["test/unit/EquivalenceInvalid.test"]
  "Invalid equivalence tests" 
  \src (t, u, k, m) -> equivalent m t u `shouldBe` False
