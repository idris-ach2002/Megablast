module MeteoreScoreSpec (spec) where

import Model.Hitbox
import Model.Meteore
import Model.Objects
import Model.Score
import SpecHelpers
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

spec :: Spec
spec = do
  describe "Meteore" $ do
    it "mkMeteore rejette hitbox, cadence ou dégâts invalides" $ do
      let cad = unsafeRight "MeteoreScoreSpec: cad" (mkCadence 1)
      mkMeteore (Disque 0 0 0) cad 1 `shouldSatisfy` isLeft
      mkMeteore (Disque 0 0 3) (Cadence 0 0) 1 `shouldSatisfy` isLeft
      mkMeteore (Disque 0 0 3) cad 0 `shouldSatisfy` isLeft

    prop "mkMeteore accepte une hitbox, une cadence et des dégâts valides" $
      forAll genMobileHitbox $ \h ->
        forAll genCadence $ \cad ->
          forAll (chooseInt (1, 10)) $ \degats ->
            isRightWith prop_inv_meteore (mkMeteore h cad degats)

    prop "mkMeteoreRond produit un météore valide pour rayon et cadence valides" $
      \(SmallInt x) (SmallInt y) (Positive r) ->
        forAll genCadence $ \cad ->
          isRightWith prop_inv_meteore (mkMeteoreRond x y r cad)

    prop "avanceMeteore descend la hitbox d'un pixel et préserve l'invariant" $
      forAll genMeteoreSimple $ \mt ->
        let mt' = avanceMeteore mt
        in prop_inv_meteore mt'
           && mtHitbox mt' == translateHitbox 0 (-1) (mtHitbox mt)
           && mtDegats mt' == mtDegats mt

    prop "finDeTourMeteore respecte sa pré/post-condition" $
      forAll genMeteoreSimple $ \mt ->
        prop_pre_finDeTourMeteore mt
        && prop_post_finDeTourMeteore mt (finDeTourMeteore mt)

    it "un météore de cadence 1 descend à chaque tour" $ do
      let cad = unsafeRight "MeteoreScoreSpec: cad" (mkCadence 1)
          mt0 = unsafeRight "MeteoreScoreSpec: mt0" (mkMeteoreRond 10 20 5 cad)
          mt1 = finDeTourMeteore mt0
          mt2 = finDeTourMeteore mt1
      mtHitbox mt1 `shouldBe` Disque 10 19 5
      mtHitbox mt2 `shouldBe` Disque 10 18 5

  describe "Score" $ do
    prop "Score généré respecte son invariant" $
      \s -> prop_inv_score (s :: Score)

    it "scoresNuls crée exactement n scores nuls pour n positif" $ do
      scoresNuls 0 `shouldBe` []
      scoresNuls (-3) `shouldBe` []
      scoresNuls 3 `shouldBe` [scoreNul, scoreNul, scoreNul]

    prop "ajouterPoints préserve l'invariant et ignore les valeurs non positives" $
      \points score ->
        prop_post_ajouterPoints points score
        && if points <= 0
             then ajouterPoints points score == (score :: Score)
             else scoreValeur (ajouterPoints points score) == scoreValeur score + points

    prop "ajouterPointsJoueuse préserve les scores et ne change pas la longueur" $
      forAll genScores $ \scores ->
        \indice points ->
          let scores' = ajouterPointsJoueuse indice points scores
          in prop_post_ajouterPointsJoueuse indice points scores
             && length scores' == length scores

    it "ajouterPointsJoueuse ignore les indices invalides" $ do
      let scores = [Score 1, Score 2]
      ajouterPointsJoueuse (-1) 10 scores `shouldBe` scores
      ajouterPointsJoueuse 3 10 scores `shouldBe` scores

    it "ajouterPointsJoueuse crédite uniquement la joueuse ciblée" $ do
      ajouterPointsJoueuse 1 10 [Score 1, Score 2, Score 3]
        `shouldBe` [Score 1, Score 12, Score 3]

    prop "scoreTotal est la somme des scores individuels" $
      forAll genScores $ \scores ->
        prop_post_scoreTotal scores
        && scoreValeur (scoreTotal scores) == sum (map scoreValeur scores)

    it "scoreEnnemiDetruit est positif et augmente par paliers de survie" $ do
      scoreEnnemiDetruit 0 `shouldBe` scoreEnnemiBase
      scoreEnnemiDetruit 1000 `shouldSatisfy` (> scoreEnnemiDetruit 999)
      scoreEnnemiDetruit 2500 `shouldSatisfy` (> scoreEnnemiDetruit 2499)
      scoreEnnemiDetruit 5000 `shouldSatisfy` (> scoreEnnemiDetruit 4999)

    prop "scoreEnnemiDetruit reste toujours au moins égal au score de base" $
      prop_scoreEnnemiDetruit_positif
