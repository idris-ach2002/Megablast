module HitboxSpec (spec) where

import Model.Hitbox
import Model.Murs
import Model.Objects (translateHitbox)
import SpecHelpers
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

murGaucheVerticalTest :: Hitbox
murGaucheVerticalTest = MurGauche [(10,0), (10,10), (10,20), (10,30)]

murDroitVerticalTest :: Hitbox
murDroitVerticalTest = MurDroit [(10,0), (10,10), (10,20), (10,30)]

murDentScieTest :: Hitbox
murDentScieTest = MurGauche [(0,0), (5,10), (0,20), (5,30)]

mkSmallRectangle :: Int -> Int -> Int -> Int -> Hitbox
mkSmallRectangle x y w0 h0 =
  let w = abs w0 `mod` 20 + 1
      h = abs h0 `mod` 20 + 1
  in Rectangle x y w h

spec :: Spec
spec = do
  describe "Hitbox: invariants et constructeurs intelligents" $ do
    prop "tout générateur de hitbox valide satisfait prop_inv_hitbox" $
      forAll arbitrary $ \h -> prop_inv_hitbox (h :: Hitbox)

    it "mkDisque rejette un rayon non strictement positif" $ do
      mkDisque 0 0 0 `shouldSatisfy` isLeft
      mkDisque 0 0 (-3) `shouldSatisfy` isLeft

    prop "mkDisque accepte exactement les rayons strictement positifs" $
      \(SmallInt x) (SmallInt y) r ->
        isRightWith prop_inv_hitbox (mkDisque x y (abs r + 1))

    it "mkRectangle rejette une largeur ou hauteur non strictement positive" $ do
      mkRectangle 0 0 0 1 `shouldSatisfy` isLeft
      mkRectangle 0 0 1 0 `shouldSatisfy` isLeft
      mkRectangle 0 0 (-1) 1 `shouldSatisfy` isLeft

    prop "mkRectangle accepte exactement les dimensions strictement positives" $
      \(SmallInt x) (SmallInt y) w h ->
        isRightWith prop_inv_hitbox (mkRectangle x y (abs w + 1) (abs h + 1))

    it "mkComposee rejette les listes vides ou singletons" $ do
      mkComposee [] `shouldSatisfy` isLeft
      mkComposee [Point 0 0] `shouldSatisfy` isLeft

    prop "mkComposee accepte une liste de deux hitbox valides ou plus" $
      forAll (listOf1 genMobileHitbox) $ \hs0 ->
        let hs = take 4 (hs0 ++ [Point 0 0])
        in length hs >= 2 ==> isRightWith prop_inv_hitbox (mkComposee hs)

    it "l'invariant des murs refuse les listes trop courtes et les ordonnées non croissantes" $ do
      prop_inv_hitbox (MurGauche []) `shouldBe` False
      prop_inv_hitbox (MurGauche [(0,0)]) `shouldBe` False
      prop_inv_hitbox (MurGauche [(0,0),(1,2),(2,1)]) `shouldBe` False
      prop_inv_hitbox (MurDroit [(0,0),(1,0)]) `shouldBe` False

    it "l'invariant des murs reste productif sur les murs infinis du sujet" $ do
      prop_inv_hitbox (mur_gauche_dents_scie 5 10) `shouldBe` True
      prop_inv_hitbox (mur_gauche_vertical 10 10) `shouldBe` True

  describe "Hitbox: collision de base" $ do
    it "deux points collisionnent seulement s'ils sont superposés" $ do
      collision (Point 1 2) (Point 1 2) `shouldBe` True
      collision (Point 1 2) (Point 2 1) `shouldBe` False

    it "Point/Rectangle : bord gauche et bas inclus, bord droit et haut exclus" $ do
      let r = Rectangle 10 20 3 4
      collision (Point 10 20) r `shouldBe` True
      collision (Point 12 23) r `shouldBe` True
      collision (Point 13 22) r `shouldBe` False
      collision (Point 11 24) r `shouldBe` False

    prop "Point/Rectangle est symétrique" $
      \(SmallInt x) (SmallInt y) (SmallInt rx) (SmallInt ry) (SmallInt w0) (SmallInt h0) ->
        let p = Point x y
            r = mkSmallRectangle rx ry w0 h0
        in collision p r == collision r p

    it "Rectangle/Rectangle détecte les recouvrements et exclut les bords seulement tangents" $ do
      collision (Rectangle 0 0 10 10) (Rectangle 9 9 4 4) `shouldBe` True
      collision (Rectangle 0 0 10 10) (Rectangle 10 0 4 4) `shouldBe` False
      collision (Rectangle 0 0 10 10) (Rectangle 0 10 4 4) `shouldBe` False

    prop "Rectangle/Rectangle est symétrique" $
      \(SmallInt x1) (SmallInt y1) (SmallInt w1) (SmallInt h1)
       (SmallInt x2) (SmallInt y2) (SmallInt w2) (SmallInt h2) ->
        let r1 = mkSmallRectangle x1 y1 w1 h1
            r2 = mkSmallRectangle x2 y2 w2 h2
        in collision r1 r2 == collision r2 r1

    it "Point/Disque inclut le centre, l'intérieur et le bord" $ do
      let d = Disque 0 0 5
      collision (Point 0 0) d `shouldBe` True
      collision (Point 3 4) d `shouldBe` True
      collision (Point 6 0) d `shouldBe` False

    prop "Point/Disque est symétrique" $
      \(SmallInt x) (SmallInt y) (SmallInt cx) (SmallInt cy) (Positive r) ->
        let p = Point x y
            d = Disque cx cy r
        in collision p d == collision d p

    it "Disque/Disque traite la tangence comme une collision" $ do
      collision (Disque 0 0 3) (Disque 6 0 3) `shouldBe` True
      collision (Disque 0 0 3) (Disque 7 0 3) `shouldBe` False

    prop "Disque/Disque est symétrique" $
      \(SmallInt x1) (SmallInt y1) (Positive r1) (SmallInt x2) (SmallInt y2) (Positive r2) ->
        let d1 = Disque x1 y1 r1
            d2 = Disque x2 y2 r2
        in collision d1 d2 == collision d2 d1

    it "Rectangle/Disque couvre l'intérieur, la tangence et l'absence de contact" $ do
      let r = Rectangle 0 0 10 10
      collision r (Disque 5 5 2) `shouldBe` True
      collision r (Disque 12 5 3) `shouldBe` True
      collision r (Disque 20 20 2) `shouldBe` False

    prop "Rectangle/Disque est symétrique" $
      \(SmallInt rx) (SmallInt ry) (SmallInt w0) (SmallInt h0)
       (SmallInt cx) (SmallInt cy) (Positive r0) ->
        let rect = mkSmallRectangle rx ry w0 h0
            disque = Disque cx cy r0
        in collision rect disque == collision disque rect

  describe "Hitbox: Composee" $ do
    it "une Composee collisionne dès qu'une composante collisionne" $ do
      let h = Composee [Rectangle 0 0 2 2, Disque 10 10 2]
      collision h (Point 1 1) `shouldBe` True
      collision h (Point 10 10) `shouldBe` True
      collision h (Point 5 5) `shouldBe` False

    prop "Q1.4: collision avec Composee de deux points implique appartenance à ces points" $
      \(SmallInt x1) (SmallInt y1) (SmallInt x2) (SmallInt y2) (SmallInt x) (SmallInt y) ->
        prop_composee2points_point (x1,y1) (x2,y2) (x,y)

    prop "collision est symétrique sur les hitbox générées" $
      forAll genMobileHitbox $ \h1 ->
        forAll genMobileHitbox $ \h2 ->
          collision h1 h2 == collision h2 h1

  describe "Hitbox: murs" $ do
    it "Point/MurGauche fonctionne sur le mur dent de scie de l'énoncé" $ do
      collision (Point 1 8) murDentScieTest `shouldBe` True
      collision (Point 5 19) murDentScieTest `shouldBe` False

    it "Point/MurGauche fonctionne avec un mur vertical" $ do
      collision (Point 8 5) murGaucheVerticalTest `shouldBe` True
      collision (Point 10 5) murGaucheVerticalTest `shouldBe` True
      collision (Point 11 5) murGaucheVerticalTest `shouldBe` False

    it "Point/MurDroit fonctionne avec un mur vertical" $ do
      collision (Point 12 5) murDroitVerticalTest `shouldBe` True
      collision (Point 10 5) murDroitVerticalTest `shouldBe` True
      collision (Point 9 5) murDroitVerticalTest `shouldBe` False

    prop "MurGauche/Point est symétrique" $
      \(SmallInt x) (SmallInt y0) ->
        let y = abs y0 `mod` 30
            p = Point x y
        in collision murGaucheVerticalTest p == collision p murGaucheVerticalTest

    prop "MurDroit/Point est symétrique" $
      \(SmallInt x) (SmallInt y0) ->
        let y = abs y0 `mod` 30
            p = Point x y
        in collision murDroitVerticalTest p == collision p murDroitVerticalTest

    it "Rectangle/MurGauche collisionne si le rectangle atteint la zone du mur" $ do
      let rect = Rectangle 8 5 3 4
      collision rect murGaucheVerticalTest `shouldBe` True
      collision murGaucheVerticalTest rect `shouldBe` True

    it "Rectangle/MurDroit collisionne si le rectangle atteint la zone du mur" $ do
      let rect = Rectangle 9 5 3 4
      collision rect murDroitVerticalTest `shouldBe` True
      collision murDroitVerticalTest rect `shouldBe` True

    prop "Rectangle/MurGauche est symétrique" $
      \(SmallInt x) (SmallInt y0) (SmallInt w0) (SmallInt h0) ->
        let y = abs y0 `mod` 25
            rect = mkSmallRectangle x y w0 h0
        in collision rect murGaucheVerticalTest == collision murGaucheVerticalTest rect

    prop "Rectangle/MurDroit est symétrique" $
      \(SmallInt x) (SmallInt y0) (SmallInt w0) (SmallInt h0) ->
        let y = abs y0 `mod` 25
            rect = mkSmallRectangle x y w0 h0
        in collision rect murDroitVerticalTest == collision murDroitVerticalTest rect

  describe "Hitbox: translation exhaustive par constructeur" $ do
    it "translateHitbox couvre Point, Disque, Rectangle, MurGauche, MurDroit et Composee" $ do
      translateHitbox 2 (-3) (Point 1 1) `shouldBe` Point 3 (-2)
      translateHitbox 2 (-3) (Disque 1 1 4) `shouldBe` Disque 3 (-2) 4
      translateHitbox 2 (-3) (Rectangle 1 1 5 6) `shouldBe` Rectangle 3 (-2) 5 6
      translateHitbox 2 (-3) (MurGauche [(0,0),(0,10)]) `shouldBe` MurGauche [(2,-3),(2,7)]
      translateHitbox 2 (-3) (MurDroit [(10,0),(10,10)]) `shouldBe` MurDroit [(12,-3),(12,7)]
      translateHitbox 2 (-3) (Composee [Point 0 0, Rectangle 1 1 2 2])
        `shouldBe` Composee [Point 2 (-3), Rectangle 3 (-2) 2 2]
