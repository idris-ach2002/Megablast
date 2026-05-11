module Main where

import qualified AlgebraicSpec
import qualified EngineSpec
import qualified HitboxSpec
import qualified MeteoreScoreSpec
import qualified ObjectsSpec
import qualified VaisseauSpec

import Test.Hspec

main :: IO ()
main = hspec $ do
  AlgebraicSpec.spec
  HitboxSpec.spec
  ObjectsSpec.spec
  VaisseauSpec.spec
  MeteoreScoreSpec.spec
  EngineSpec.spec
