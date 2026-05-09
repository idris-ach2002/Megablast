{-# LANGUAGE OverloadedStrings #-}

module Model.Engine.Collisions where

import Model.Engine.Types
import Model.Hitbox
import Model.Meteore
import Model.Objects
import Model.Score

-- | Une joueuse est encore jouable seulement s'il lui reste au moins un essai
--   et au moins un point de vie.
--
--   Convention :
--   - essais > 0 : la joueuse peut encore jouer ;
--   - essais == 0 : la joueuse est éliminée.
--
-- Important en multijoueur : on ne doit jamais compacter la liste
-- mJoueuses après une élimination. Les indices de cette liste sont les
-- identités stables des joueuses : indice 0 = J1, indice 1 = J2.
-- Si J1 disparaît physiquement de la liste, J2 prend l'indice 0 et récupère
-- les touches ainsi que le score de J1.
joueuseEncoreEnJeu :: VaisseauJoueuse -> Bool
joueuseEncoreEnJeu v =
  vjPv v > 0 && vjEssais v > 0

joueusesActives :: [VaisseauJoueuse] -> [VaisseauJoueuse]
joueusesActives =
  filter joueuseEncoreEnJeu

-- | Conservé pour compatibilité avec le reste du moteur, mais cette fonction
--   ne supprime plus les joueuses éliminées. Elle stabilise les slots joueurs.
--   La disparition visuelle se fait côté vue avec joueusesActives.
supprimerJoueusesEliminees :: [VaisseauJoueuse] -> [VaisseauJoueuse]
supprimerJoueusesEliminees =
  id

prop_pre_supprimerJoueusesEliminees :: [VaisseauJoueuse] -> Bool
prop_pre_supprimerJoueusesEliminees =
  all prop_inv_vaisseau

prop_post_supprimerJoueusesEliminees :: [VaisseauJoueuse] -> [VaisseauJoueuse] -> Bool
prop_post_supprimerJoueusesEliminees js js' =
     all prop_inv_vaisseau js'
  && length js' == length js

prop_partie_en_cours :: Moteur -> Bool
prop_partie_en_cours m =
  any joueuseEncoreEnJeu (mJoueuses m)

pvRespawnJoueuse :: Int
pvRespawnJoueuse =
  pvMaxJoueuse

distanceRepousseMur :: Int
distanceRepousseMur = 10

limiteCorrectionMur :: Int
limiteCorrectionMur = 80

repousseVaisseauN :: Int -> Direction -> VaisseauJoueuse -> VaisseauJoueuse
repousseVaisseauN n d v
  | n <= 0    = v
  | otherwise = repousseVaisseauN (n - 1) d (repousseVaisseau d v)

repousseHorsMur :: Hitbox -> Direction -> VaisseauJoueuse -> VaisseauJoueuse
repousseHorsMur mur d =
  ajouterImpulsion . corriger limiteCorrectionMur
  where
    corriger 0 v = v
    corriger n v
      | collision (vjHitbox v) mur = corriger (n - 1) (repousseVaisseau d v)
      | otherwise                  = v

    ajouterImpulsion v =
      repousseVaisseauN distanceRepousseMur d v

reapparaitreJoueuse :: VaisseauJoueuse -> VaisseauJoueuse
reapparaitreJoueuse v
  | vjPv v > 0 =
      v

  | vjEssais v <= 0 =
      v { vjPv = 0, vjEssais = 0 }

  | vjEssais v == 1 =
      v { vjPv = 0, vjEssais = 0 }

  | otherwise =
      v { vjPv = pvRespawnJoueuse, vjEssais = vjEssais v - 1 }

prop_post_reapparaitreJoueuse :: VaisseauJoueuse -> Bool
prop_post_reapparaitreJoueuse v =
  let v' = reapparaitreJoueuse v
  in     prop_inv_vaisseau v'
      && case () of
           _ | vjPv v > 0 ->
                 v' == v

             | vjEssais v <= 1 ->
                    vjPv v' == 0
                 && vjEssais v' == 0

             | otherwise ->
                    vjPv v' == pvRespawnJoueuse
                 && vjEssais v' == vjEssais v - 1

-- | Applique un dégât à une joueuse puis gère immédiatement la transition vers
--   l'état "essai consommé / respawn" quand c'est nécessaire.
encaisserDegatJoueuse :: VaisseauJoueuse -> VaisseauJoueuse
encaisserDegatJoueuse = reapparaitreJoueuse . subirDegat

encaisserDegatsJoueuse :: Int -> VaisseauJoueuse -> VaisseauJoueuse
encaisserDegatsJoueuse n v
  | n <= 0    = v
  | otherwise = encaisserDegatsJoueuse (n - 1) (encaisserDegatJoueuse v)

joueuseToucheePar :: VaisseauJoueuse -> Projectile -> (VaisseauJoueuse, Bool)
joueuseToucheePar v p
  | not (joueuseEncoreEnJeu v) =
      (v, False)
  | prOwner p == TirEnnemi
    && collision (vjHitbox v) (prHitbox p) = (encaisserDegatJoueuse v, True)
  | otherwise                              = (v, False)

joueuseToucheeMeteore :: VaisseauJoueuse -> Meteore -> (VaisseauJoueuse, Bool)
joueuseToucheeMeteore v mt
  | not (joueuseEncoreEnJeu v) =
      (v, False)
  | collision (vjHitbox v) (mtHitbox mt) =
      (encaisserDegatsJoueuse (mtDegats mt) v, True)
  | otherwise =
      (v, False)

ennemiToucheePar :: Ennemi -> Projectile -> (Maybe Ennemi, Bool)
ennemiToucheePar e p =
  let (me, touche, _) = ennemiToucheeParAvecMort e p
  in (me, touche)

ennemiToucheeParAvecMort :: Ennemi -> Projectile -> (Maybe Ennemi, Bool, Bool)
ennemiToucheeParAvecMort e p
  | estTirJoueuse p
    && collision (eHitbox e) (prHitbox p) =
        let (PV pv) = ePV e
            pv' = pv - 1
            mort = pv' <= 0
        in (if mort then Nothing else Just (e { ePV = PV pv' }), True, mort)

  | otherwise =
      (Just e, False, False)

joueuseToucheeObstacle :: VaisseauJoueuse -> Obstacle -> VaisseauJoueuse
joueuseToucheeObstacle v o
  | not (joueuseEncoreEnJeu v)           = v
  | collision (vjHitbox v) (obsHitbox o) = repousseVaisseau Haut v
  | otherwise                            = v

joueuseToucheeEnnemi :: VaisseauJoueuse -> Ennemi -> VaisseauJoueuse
joueuseToucheeEnnemi v e
  | not (joueuseEncoreEnJeu v)        = v
  | collision (vjHitbox v) (eHitbox e) = repousseVaisseau Haut (encaisserDegatJoueuse v)
  | otherwise                          = v

joueuseToucheeMurGauche :: Hitbox -> VaisseauJoueuse -> VaisseauJoueuse
joueuseToucheeMurGauche mur v
  | not (joueuseEncoreEnJeu v) = v
  | collision (vjHitbox v) mur = repousseHorsMur mur Gauche v
  | otherwise                  = v

joueuseToucheeMurDroit :: Hitbox -> VaisseauJoueuse -> VaisseauJoueuse
joueuseToucheeMurDroit mur v
  | not (joueuseEncoreEnJeu v) = v
  | collision (vjHitbox v) mur = repousseHorsMur mur Droite v
  | otherwise                  = v

joueuseToucheeMurs :: MursNiveau -> VaisseauJoueuse -> VaisseauJoueuse
joueuseToucheeMurs murs =
  joueuseToucheeMurDroit (mnMurDroit murs)
  . joueuseToucheeMurGauche (mnMurGauche murs)

resoudreCollisions :: Moteur -> Moteur
resoudreCollisions m =
  let
    joueusesAuDepart =
      supprimerJoueusesEliminees (mJoueuses m)

    (jous1, projsApresJoueuses) =
      foldr appliquerProjSurJoueuses (joueusesAuDepart, []) (mProjectiles m)

    appliquerProjSurJoueuses p (js, acc) =
      let (js', consomme) = parcourirJoueuses p js
      in if consomme
           then (js', acc)
           else (js', p : acc)

    parcourirJoueuses _ [] =
      ([], False)

    parcourirJoueuses p (j:js) =
      let (j', hit) = joueuseToucheePar j p
      in if hit
           then (supprimerJoueusesEliminees (j' : js), True)
           else
             let (js', found) = parcourirJoueuses p js
             in (j : js', found)

    m1 =
      m { mJoueuses = jous1
        , mProjectiles = projsApresJoueuses
        }

    (enns2, projsRestes, scoresGagnes) =
      foldr appliquerProjSurEnnemis (mEnnemis m1, [], mScores m1) (mProjectiles m1)

    appliquerProjSurEnnemis p (es, acc, scoresAcc) =
      let (es', consomme, ennemiDetruit) = parcourirEnnemis p es
          scoresAcc' =
            if ennemiDetruit
              then crediterScoreProjectile p (scoreEnnemiDetruit (mTour m1)) scoresAcc
              else scoresAcc
      in if consomme
           then (es', acc, scoresAcc')
           else (es', p : acc, scoresAcc')

    crediterScoreProjectile p points scores =
      case indiceJoueuseProjectile p of
        Nothing ->
          scores

        Just indiceJoueuse ->
          ajouterPointsJoueuse indiceJoueuse points scores

    parcourirEnnemis _ [] =
      ([], False, False)

    parcourirEnnemis p (e:es) =
      let (me', hit, mort) = ennemiToucheeParAvecMort e p
          es' = maybe es (:es) me'
      in if hit
           then (es', True, mort)
           else
             let (es'', found, mortTrouvee) = parcourirEnnemis p es
             in (e : es'', found, mortTrouvee)

    m2 =
      m1 { mEnnemis = enns2
         , mProjectiles = projsRestes
         , mScores = scoresGagnes
         , mScore = scoreTotal scoresGagnes
         }
    (meteoresRestants, jousApresMeteores) =
      foldr appliquerMeteoreSurJoueuses ([], mJoueuses m2) (mMeteores m2)

    appliquerMeteoreSurJoueuses mt (acc, js) =
      let (js', touche) = parcourirJoueusesMeteore mt js
      in if touche
           then (acc, js')
           else (mt : acc, js')

    parcourirJoueusesMeteore _ [] =
      ([], False)

    parcourirJoueusesMeteore mt (j:js) =
      let (j', hit) = joueuseToucheeMeteore j mt
      in if hit
           then (supprimerJoueusesEliminees (j' : js), True)
           else
             let (js', found) = parcourirJoueusesMeteore mt js
             in (j : js', found)

    m3 =
      m2 { mMeteores = meteoresRestants
         , mJoueuses = jousApresMeteores
         }

    jous3 =
      [ foldl joueuseToucheeObstacle j (mObstacles m3)
      | j <- mJoueuses m3
      ]

    jous4 =
      supprimerJoueusesEliminees $
        map (joueuseToucheeMurs (mMurs m3)) jous3

  in m3 { mJoueuses = jous4 }