module Main where

import qualified EngineSpec
import qualified HitboxSpec
import qualified MeteoreScoreSpec
import qualified ObjectsSpec
import qualified VaisseauSpec

import Test.Hspec

main :: IO ()
main = hspec $ do
  HitboxSpec.spec
  ObjectsSpec.spec
  VaisseauSpec.spec
  MeteoreScoreSpec.spec
  EngineSpec.spec
