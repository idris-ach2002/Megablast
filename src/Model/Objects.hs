{- HLINT ignore "Use camelCase" -}
{-# LANGUAGE OverloadedStrings #-}

module Model.Objects where

import Data.Text (Text)
import Model.Hitbox
import Model.Lib
import Model.VaisseauForme

---------------------------------------------------------------------------------
--- Partie 3 : Obstacles / Projectiles / Tour
---------------------------------------------------------------------------------

data Direction = Droite | Haut | Gauche | Bas
  deriving (Eq, Show, Enum, Bounded)

dirVector :: Direction -> (Int, Int)
dirVector Haut   = (0, 1)
dirVector Bas    = (0, -1)
dirVector Gauche = (-1, 0)
dirVector Droite = (1, 0)

-- Cadence: attente>0, restant ∈ [0..attente-1], restant==0 => bouge maintenant
-- attente = N (tous les combien de tours on bouge)
-- restant = compteur interne "combien de tours avant le prochain mouvement"
data Cadence = Cadence { attente :: Int, restant :: Int }
  deriving (Eq, Show)

prop_inv_cadence :: Cadence -> Bool
prop_inv_cadence (Cadence a r) = a > 0 && 0 <= r && r < a

mkCadence :: Int -> Either Text Cadence
mkCadence a
  | a > 0     = Right (Cadence a 0)
  | otherwise = Left ("attente doit être > 0, reçu " <> tshow a)

-- si restant == 0:
-- on doit bouger ce tour-ci (True)
-- puis on remet le compteur à a-1 (il faudra attendre a-1 tours avant le prochain mouvement)
tickCadence :: Cadence -> (Bool, Cadence)
tickCadence c@(Cadence a r)
  | not (prop_inv_cadence c) = error "tickCadence: invariant cadence violé"
  | r == 0    = (True,  Cadence a (a - 1))
  | otherwise = (False, Cadence a (r - 1))

prop_pre_tickCadence :: Cadence -> Bool
prop_pre_tickCadence = prop_inv_cadence

prop_post_tickCadence :: Cadence -> (Bool, Cadence) -> Bool
prop_post_tickCadence (Cadence a r) (moveNow, Cadence a' r') =
  a' == a
  && prop_inv_cadence (Cadence a' r')
  && if r == 0
       then moveNow && r' == a - 1
       else (not moveNow) && r' == r - 1

translateHitbox :: Int -> Int -> Hitbox -> Hitbox
translateHitbox dx dy (Point x y)            = Point (x + dx) (y + dy)
translateHitbox dx dy (Disque xc yc r)       = Disque (xc + dx) (yc + dy) r
translateHitbox dx dy (Rectangle x y w h)    = Rectangle (x + dx) (y + dy) w h
translateHitbox dx dy (MurGauche l)          = MurGauche (map (\(x, y) -> (x+dx, y+dy)) l)
translateHitbox dx dy (MurDroit l)           = MurDroit (map (\(x, y) -> (x+dx, y+dy)) l)
translateHitbox dx dy (Composee hs)          = Composee (map (translateHitbox dx dy) hs)

newtype Obstacle = Obstacle { obsHitbox :: Hitbox }
  deriving (Eq, Show)

prop_inv_obstacle :: Obstacle -> Bool
prop_inv_obstacle (Obstacle h) = prop_inv_hitbox h

mkObstacle :: Hitbox -> Either Text Obstacle
mkObstacle h
  | prop_inv_hitbox h = Right (Obstacle h)
  | otherwise         = Left "Hitbox invalide pour obstacle"

defileObstacle :: Obstacle -> Obstacle
defileObstacle (Obstacle h) = Obstacle (translateHitbox 0 (-1) h)

data ProjectileOwner = TirJoueuse | TirEnnemi
  deriving (Eq, Show, Enum, Bounded)

data Projectile = Projectile
  { prHitbox  :: Hitbox
  , prDir     :: Direction
  , prCadence :: Cadence
  , prOwner   :: ProjectileOwner
  } deriving (Eq, Show)

prop_inv_projectile :: Projectile -> Bool
prop_inv_projectile p =
  prop_inv_hitbox (prHitbox p) && prop_inv_cadence (prCadence p)

mkProjectile :: Hitbox -> Direction -> Cadence -> ProjectileOwner -> Either Text Projectile
mkProjectile h d c o
  | prop_inv_hitbox h && prop_inv_cadence c = Right (Projectile h d c o)
  | otherwise = Left "Projectile invalide (hitbox ou cadence)"

avanceProjectile :: Projectile -> Projectile
avanceProjectile p =
  let (dx, dy) = dirVector (prDir p)
  in p { prHitbox = translateHitbox dx dy (prHitbox p) }

prop_pre_avanceProjectile :: Projectile -> Bool
prop_pre_avanceProjectile = prop_inv_projectile

prop_post_avanceProjectile :: Projectile -> Projectile -> Bool
prop_post_avanceProjectile p p' =
  prop_inv_projectile p'
  && prDir p' == prDir p
  && prCadence p' == prCadence p
  && prOwner p' == prOwner p

finDeTourProjectile :: Projectile -> Projectile
finDeTourProjectile p
  | not (prop_inv_projectile p) = error "finDeTourProjectile: invariant projectile violé"
  | otherwise =
      let (moveNow, c') = tickCadence (prCadence p)
          p' = p { prCadence = c' }
      in if moveNow then avanceProjectile p' else p'

prop_pre_finDeTourProjectile :: Projectile -> Bool
prop_pre_finDeTourProjectile = prop_inv_projectile

prop_post_finDeTourProjectile :: Projectile -> Projectile -> Bool
prop_post_finDeTourProjectile p p' =
  prop_inv_projectile p'
  && prDir p' == prDir p
  && prOwner p' == prOwner p

-- scrolling global: on tick une cadence, si moveNow alors tous les obstacles descendent
finDeTourObstacles :: Cadence -> [Obstacle] -> (Cadence, [Obstacle])
finDeTourObstacles cad obs
  | not (prop_inv_cadence cad) = error "finDeTourObstacles: invariant cadence violé"
  | otherwise =
      let (moveNow, cad') = tickCadence cad
          obs' = if moveNow then map defileObstacle obs else obs
      in (cad', obs')

prop_pre_finDeTourObstacles :: Cadence -> [Obstacle] -> Bool
prop_pre_finDeTourObstacles cad obs =
  prop_inv_cadence cad && all prop_inv_obstacle obs

prop_post_finDeTourObstacles :: Cadence -> [Obstacle] -> (Cadence, [Obstacle]) -> Bool
prop_post_finDeTourObstacles _ _ (cad', obs') =
  prop_inv_cadence cad' && all prop_inv_obstacle obs'

---------------------------------------------------------------------------------
--- Partie 4 : Joueuses
---------------------------------------------------------------------------------

data Action = Deplacer Direction | Tirer | Attendre
  deriving (Eq, Show)

data VaisseauJoueuse = VaisseauJoueuse
  { vjForme   :: PartiesVaisseau
  , vjPv      :: Int
  , vjEssais  :: Int
  , vjCadence :: Cadence
  } deriving (Eq, Show)

vjHitbox :: VaisseauJoueuse -> Hitbox
vjHitbox = hitboxPartiesVaisseau . vjForme

vjHitboxTir :: VaisseauJoueuse -> Hitbox
vjHitboxTir = hitboxTirPartiesVaisseau . vjForme

prop_inv_vaisseau :: VaisseauJoueuse -> Bool
prop_inv_vaisseau v =
  prop_inv_partiesVaisseau (vjForme v)
  && prop_inv_hitbox (vjHitbox v)
  && vjPv v >= 0
  && vjEssais v >= 0
  && prop_inv_cadence (vjCadence v)

mkVaisseauJoueuse :: PartiesVaisseau -> Int -> Int -> Cadence -> Either Text VaisseauJoueuse
mkVaisseauJoueuse forme pv essais cad
  | not (prop_inv_partiesVaisseau forme) = Left "Vaisseau: forme invalide"
  | pv < 0                               = Left "Vaisseau: Points de vie negatifs"
  | essais < 0                           = Left "Vaisseau: Nombre d'essais negatif"
  | not (prop_inv_cadence cad)           = Left "Vaisseau: Cadence invalide"
  | otherwise                            = Right (VaisseauJoueuse forme pv essais cad)

deplaceVaisseau :: Direction -> VaisseauJoueuse -> VaisseauJoueuse
deplaceVaisseau d v =
  let (dx, dy) = dirVector d
  in v { vjForme = translatePartiesVaisseau dx dy (vjForme v) }

prop_pre_deplaceVaisseau :: Direction -> VaisseauJoueuse -> Bool
prop_pre_deplaceVaisseau _ = prop_inv_vaisseau

prop_post_deplaceVaisseau :: Direction -> VaisseauJoueuse -> VaisseauJoueuse -> Bool
prop_post_deplaceVaisseau _ v v' =
     prop_inv_vaisseau v'
  && vjPv v' == vjPv v
  && vjEssais v' == vjEssais v
  && vjCadence v' == vjCadence v

-- Si on est repoussé on se déplace vers la direction opposée.
repousseVaisseau :: Direction -> VaisseauJoueuse -> VaisseauJoueuse
repousseVaisseau d v =
  let (dx, dy) = dirVector d
  in v { vjForme = translatePartiesVaisseau (-dx) (-dy) (vjForme v) }

prop_pre_repousseVaisseau :: Direction -> VaisseauJoueuse -> Bool
prop_pre_repousseVaisseau _  = prop_inv_vaisseau

prop_post_repousseVaisseau :: Direction -> VaisseauJoueuse -> VaisseauJoueuse -> Bool
prop_post_repousseVaisseau d v v' =
     prop_inv_vaisseau v'
  && deplaceVaisseau d v' == v

tirVaisseau :: VaisseauJoueuse -> Cadence -> Projectile
tirVaisseau v cadProj =
  Projectile
    { prHitbox  = vjHitboxTir v
    , prDir     = Haut
    , prCadence = cadProj
    , prOwner   = TirJoueuse
    }

prop_pre_tirVaisseau :: VaisseauJoueuse -> Cadence -> Bool
prop_pre_tirVaisseau v cadProj =
  prop_inv_vaisseau v && prop_inv_cadence cadProj

prop_post_tirVaisseau :: VaisseauJoueuse -> Cadence -> Projectile -> Bool
prop_post_tirVaisseau _ _ p =
     prop_inv_projectile p
  && prDir p == Haut
  && prOwner p == TirJoueuse

subirDegat :: VaisseauJoueuse -> VaisseauJoueuse
subirDegat v = v { vjPv = max 0 (vjPv v - 1) }

prop_pre_subirDegat :: VaisseauJoueuse -> Bool
prop_pre_subirDegat = prop_inv_vaisseau

prop_post_subirDegat :: VaisseauJoueuse -> VaisseauJoueuse -> Bool
prop_post_subirDegat v v' =
     prop_inv_vaisseau v'
  && vjPv v' == max 0 (vjPv v - 1)
  && vjEssais v' == vjEssais v

---------------------------------------------------------------------------------
--- Partie 5 : Ennemis
---------------------------------------------------------------------------------

data Oracle
  = Scripted [Action] Int
  | FromInt
  deriving (Eq, Show)

prop_inv_oracle :: Oracle -> Bool
prop_inv_oracle (Scripted as i) = not (null as) && i >= 0
prop_inv_oracle FromInt         = True

oracleStep :: Oracle -> Int -> (Action, Oracle)
oracleStep o _
  | not (prop_inv_oracle o) = error "oracleStep: invariant oracle violé"
oracleStep (Scripted as i) _ =
  let k = i `mod` length as
  in (as !! k, Scripted as (i + 1))
oracleStep FromInt n = (actionFromInt n, FromInt)

prop_pre_oracleStep :: Oracle -> Int -> Bool
prop_pre_oracleStep o _ = prop_inv_oracle o

prop_post_oracleStep :: Oracle -> Int -> (Action, Oracle) -> Bool
prop_post_oracleStep _ _ (_a, o') = prop_inv_oracle o'

actionFromInt :: Int -> Action
actionFromInt n =
  case abs n `mod` 5 of
    0 -> Attendre
    1 -> Tirer
    2 -> Deplacer Gauche
    3 -> Deplacer Droite
    _ -> Deplacer Bas

newtype PV = PV Int
  deriving (Eq, Show, Ord)

prop_inv_pv :: PV -> Bool
prop_inv_pv (PV x) = x > 0

mkPV :: Int -> Either Text PV
mkPV x
  | x > 0     = Right (PV x)
  | otherwise = Left ("PV doit être > 0, reçu " <> tshow x)

data Ennemi = Ennemi
  { eHitbox   :: Hitbox
  , ePV       :: PV
  , eOracle   :: Oracle
  , eCadTir   :: Cadence
  } deriving (Eq, Show)

prop_inv_ennemi :: Ennemi -> Bool
prop_inv_ennemi e =
  prop_inv_hitbox (eHitbox e)
  && prop_inv_pv (ePV e)
  && prop_inv_oracle (eOracle e)
  && prop_inv_cadence (eCadTir e)

mkEnnemi :: Hitbox -> PV -> Oracle -> Cadence -> Either Text Ennemi
mkEnnemi h pv o cad
  | prop_inv_hitbox h && prop_inv_pv pv && prop_inv_oracle o && prop_inv_cadence cad
      = Right (Ennemi h pv o cad)
  | otherwise = Left "Ennemi invalide (hitbox/PV/oracle/cadence)"

-- fin de tour ennemi: renvoie (ennemi mis à jour, projectile éventuellement créé)
finDeTourEnnemi :: Int -> Ennemi -> (Ennemi, Maybe Projectile)
finDeTourEnnemi rnd e
  | not (prop_inv_ennemi e) = error "finDeTourEnnemi: invariant ennemi violé"
  | otherwise =
      let (act, o') = oracleStep (eOracle e) rnd
          e0 = e { eOracle = o' }
      in case act of
          Attendre -> (e0, Nothing)
          Deplacer d ->
            let (dx,dy) = dirVector d
            in (e0 { eHitbox = translateHitbox dx dy (eHitbox e0) }, Nothing)
          Tirer ->
            let (shootNow, cad') = tickCadence (eCadTir e0)
                e1 = e0 { eCadTir = cad' }
            in if shootNow
                 then
                   let hShot = eHitbox e1
                       p = Projectile hShot Bas (Cadence 1 0) TirEnnemi
                   in (e1, Just p)
                 else (e1, Nothing)

prop_pre_finDeTourEnnemi :: Int -> Ennemi -> Bool
prop_pre_finDeTourEnnemi _ = prop_inv_ennemi

prop_post_finDeTourEnnemi :: Int -> Ennemi -> (Ennemi, Maybe Projectile) -> Bool
prop_post_finDeTourEnnemi _ _ (e', mp) =
  prop_inv_ennemi e' && maybe True prop_inv_projectile mp
