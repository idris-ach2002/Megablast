module HitboxSpec (spec) where

import SpecHelpers
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Hitbox

--  Mur vertical fini utilisé dans les tests.
murGaucheVerticalTest :: Hitbox
murGaucheVerticalTest = MurGauche [(10,0), (10,10), (10,20), (10,30)]

--  Variante symétrique du mur vertical précédent.
murDroitVerticalTest :: Hitbox
murDroitVerticalTest = MurDroit [(10,0), (10,10), (10,20), (10,30)]

--  Préfixe fini du mur "en dent de scie" de l'énoncé.
murDentScieTest :: Hitbox
murDentScieTest = MurGauche [(0,0), (5,10), (0,20), (5,30)]

--  Construit un rectangle de petite taille positive à partir de petits entiers.
-- On borne volontairement largeur et hauteur pour garder des cas simples
mkSmallRectangle :: Int -> Int -> Int -> Int -> Hitbox
mkSmallRectangle x y w0 h0 =
  let w = abs w0 `mod` 20 + 1
      h = abs h0 `mod` 20 + 1
  in Rectangle x y w h

spec :: Spec
spec = do
  describe "Hitbox: collisions élémentaires" $ do
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

    it "Disque/Disque : deux disques tangents sont en collision" $ do
      let d1 = Disque 0 0 3
          d2 = Disque 6 0 3
      collision d1 d2 `shouldBe` True

    it "Disque/Disque : deux disques disjoints ne collisionnent pas" $ do
      let d1 = Disque 0 0 3
          d2 = Disque 7 0 3
      collision d1 d2 `shouldBe` False

    prop "Disque/Disque est symétrique" $
      \(SmallInt x1) (SmallInt y1) (SmallInt r10) (SmallInt x2) (SmallInt y2) (SmallInt r20) ->
        let r1 = abs r10 `mod` 20 + 1
            r2 = abs r20 `mod` 20 + 1
            d1 = Disque x1 y1 r1
            d2 = Disque x2 y2 r2
        in collision d1 d2 == collision d2 d1

    it "Rectangle/Disque : un disque entièrement dedans collisionne" $ do
      let r = Rectangle 0 0 10 10
          d = Disque 5 5 2
      collision r d `shouldBe` True

    it "Rectangle/Disque : un disque tangent à un côté collisionne" $ do
      let r = Rectangle 0 0 10 10
          d = Disque 12 5 3
      collision r d `shouldBe` True

    it "Rectangle/Disque : un disque tangent à un coin collisionne" $ do
      let r = Rectangle 0 0 10 10
          d = Disque 12 12 3
      collision r d `shouldBe` True

    it "Rectangle/Disque : un disque lointain ne collisionne pas" $ do
      let r = Rectangle 0 0 10 10
          d = Disque 20 20 2
      collision r d `shouldBe` False

    prop "Rectangle/Disque est symétrique" $
      \(SmallInt rx) (SmallInt ry) (SmallInt w0) (SmallInt h0)
       (SmallInt xc) (SmallInt yc) (SmallInt r0) ->
        let rect = mkSmallRectangle rx ry w0 h0
            rayon = abs r0 `mod` 20 + 1
            disque = Disque xc yc rayon
        in collision rect disque == collision disque rect

  describe "Hitbox: collisions avec Composee" $ do
    it "Une Composee collisionne si une de ses composantes collisionne" $ do
      let h = Composee [Rectangle 0 0 2 2, Disque 10 10 2]
      collision h (Point 1 1) `shouldBe` True
      collision h (Point 10 10) `shouldBe` True
      collision h (Point 5 5) `shouldBe` False

    prop "Q1.4: si collision avec Composee[2 points], alors c'est un des 2 points" $
      \(SmallInt x1) (SmallInt y1) (SmallInt x2) (SmallInt y2) (SmallInt x) (SmallInt y) ->
        prop_composee2points_point (x1,y1) (x2,y2) (x,y)

  describe "Hitbox: collisions avec les murs" $ do
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

    it "Rectangle/MurGauche ne collisionne pas si le rectangle reste strictement à droite" $ do
      let rect = Rectangle 11 5 3 4
      collision rect murGaucheVerticalTest `shouldBe` False
      collision murGaucheVerticalTest rect `shouldBe` False

    it "Rectangle/MurDroit collisionne si le rectangle atteint la zone du mur" $ do
      let rect = Rectangle 10 5 3 4
      collision rect murDroitVerticalTest `shouldBe` True
      collision murDroitVerticalTest rect `shouldBe` True

    it "Rectangle/MurDroit ne collisionne pas si le rectangle reste strictement à gauche" $ do
      let rect = Rectangle 6 5 3 4
      collision rect murDroitVerticalTest `shouldBe` False
      collision murDroitVerticalTest rect `shouldBe` False

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
