module BisimulationValidSpec (spec) where

import Syntax.Module qualified as M
import UI.Error ( showErrors )
import Validation.Kinding ( runCheck )
import Validation.Subtyping.Compare (bisimilar)
import UnitSpecUtils ( mkComparisonSpec )

import Data.Map.Strict qualified as Map
import Debug.Trace ( trace )
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = mkComparisonSpec
  ["test/unit/EquivalenceValid.test"]
  "Valid type equivalence tests" 
  \src (t, u, k, m) ->  bisimilar m t u `shouldBe` True