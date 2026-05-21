# MegablastXenon2

Projet PAF 2026 — Sorbonne Université  
Jeu vidéo de type **shoot'em up vertical** inspiré de *Xenon 2: Megablast*, développé en **Haskell** avec **Gloss**, selon une architecture **MVC** et une démarche de programmation sûre : invariants, préconditions, postconditions, tests HSpec et tests de propriétés QuickCheck.

---

## Sommaire

- [Présentation](#présentation)
- [Fonctionnalités](#fonctionnalités)
- [Contrôles](#contrôles)
- [Installation](#installation)
- [Lancer le jeu](#lancer-le-jeu)
- [Lancer les tests](#lancer-les-tests)
- [Architecture du projet](#architecture-du-projet)
- [Organisation MVC](#organisation-mvc)
- [Modules importants](#modules-importants)
- [Tests et propriétés](#tests-et-propriétés)
- [Bugs corrigés et choix techniques](#bugs-corrigés-et-choix-techniques)
- [Dépendances](#dépendances)
- [Dépannage](#dépannage)
- [Licence](#licence)

---

## Présentation

`MegablastXenon2` est un jeu de tir à défilement vertical. Le joueur contrôle un vaisseau spatial, évite les murs et les ennemis, tire des projectiles, marque des points et survit jusqu'à la fin de la partie.

Le projet ne se limite pas à une implémentation graphique. Il met surtout en avant une modélisation fonctionnelle sûre :

- séparation stricte entre le **modèle pur**, le **contrôleur d'événements** et la **vue Gloss** ;
- types algébriques pour représenter les états du jeu ;
- constructeurs intelligents pour empêcher la création de valeurs invalides ;
- contrats sous forme de fonctions `prop_inv_*`, `prop_pre_*` et `prop_post_*` ;
- tests unitaires et tests génératifs par propriétés ;
- absence d'état mutable dans le modèle ;
- moteur de jeu évoluant par transitions pures.

---

## Fonctionnalités

Le jeu implémente notamment :

- écran d'accueil ;
- mode **solo** ;
- mode **duo local** ;
- boucle de jeu Gloss à 60 FPS ;
- moteur logique cadencé indépendamment du rendu ;
- défilement vertical ;
- murs gauche et droit en dents de scie ;
- collisions entre joueurs, murs, ennemis et projectiles ;
- projectiles joueurs et projectiles ennemis ;
- ennemis avec comportement automatique ;
- apparition dynamique d'objets ;
- météores ;
- score global et score par joueur ;
- écran de fin de partie ;
- gestion de la pause et du redémarrage.

---

## Contrôles

### Écran d'accueil

| Touche | Action |
|---|---|
| `1` | Lancer une partie solo |
| `2` | Lancer une partie duo local |

### Partie

| Joueur | Déplacement | Tir |
|---|---|---|
| Joueur 1 | Flèches directionnelles | `Entrée` |
| Joueur 2 | `Z`, `Q`, `S`, `D` | `Espace` |

### Commandes globales

| Touche | Action |
|---|---|
| `P` | Mettre en pause / reprendre |
| `R` | Réinitialiser la partie courante |
| `Échap` | Revenir à l'écran d'accueil |
| `Entrée` sur l'écran Game Over | Revenir à l'écran d'accueil |

---

## Installation

Le projet utilise **Stack**.

```bash
git clone https://github.com/idris-ach2002/Megablast.git
cd MegablastXenon2
stack build
```

La configuration actuelle du projet utilise le resolver défini dans `stack.yaml` et suppose qu'un compilateur GHC compatible est disponible localement, car `system-ghc: true` et `install-ghc: false` sont activés.

Pour vérifier l'environnement :

```bash
stack --version
ghc --version
```

Si Stack signale une erreur de compilateur absent ou incompatible, deux solutions sont possibles :

```bash
# Option 1 : installer le GHC attendu par le resolver Stack
stack setup

# Option 2 : modifier stack.yaml pour laisser Stack installer GHC automatiquement
# remplacer install-ghc: false par install-ghc: true
```

---

## Lancer le jeu

```bash
stack run
```

Le point d'entrée est :

```text
app/Main.hs
```

La fonction `main` charge les ressources graphiques, initialise l'état applicatif, puis lance la boucle Gloss avec :

- une fonction de dessin ;
- une fonction de gestion d'événements ;
- une fonction de simulation temporelle.

Les fichiers du dossier `assets/` doivent rester présents à la racine du projet, car ils sont chargés au lancement de l'application.

---

## Lancer les tests

```bash
stack test
```

Le projet contient une suite de tests HSpec et QuickCheck répartie dans `test/`.

Répartition des tests déclarés :

| Fichier | Nombre de tests |
|---|---:|
| `AlgebraicSpec.hs` | 21 |
| `EngineSpec.hs` | 54 |
| `HitboxSpec.hs` | 33 |
| `MeteoreScoreSpec.hs` | 15 |
| `ObjectsSpec.hs` | 29 |
| `VaisseauSpec.hs` | 17 |
| **Total** | **169** |

---

## Architecture du projet

```text
.
├── app
│   └── Main.hs
├── assets
│   ├── background_loop_open_640x1792.png
│   ├── background_loop_with_walls_640x1792.png
│   ├── enemy_*.png
│   ├── player_ship_*.png
│   ├── projectile_*.png
│   ├── wall_left_112x1792.png
│   └── wall_right_112x1792.png
├── src
│   ├── Controller
│   │   ├── AppController.hs
│   │   └── Controller.hs
│   ├── Model
│   │   ├── Engine.hs
│   │   ├── Engine
│   │   │   ├── Collisions.hs
│   │   │   ├── Commands.hs
│   │   │   ├── DynamicSpawn.hs
│   │   │   ├── EnnemiAI.hs
│   │   │   ├── EnnemiCollisions.hs
│   │   │   ├── EnnemiSpawn.hs
│   │   │   ├── GameConfig.hs
│   │   │   ├── Murs.hs
│   │   │   ├── Properties.hs
│   │   │   ├── Step.hs
│   │   │   └── Types.hs
│   │   ├── Hitbox.hs
│   │   ├── Meteore.hs
│   │   ├── Murs.hs
│   │   ├── Objects.hs
│   │   ├── Score.hs
│   │   └── VaisseauForme.hs
│   └── View
│       ├── AccueilView.hs
│       ├── AppView.hs
│       ├── Assets.hs
│       ├── Background.hs
│       ├── GameOverView.hs
│       ├── HUDView.hs
│       ├── MurView.hs
│       └── View.hs
├── test
│   ├── AlgebraicSpec.hs
│   ├── EngineSpec.hs
│   ├── HitboxSpec.hs
│   ├── MeteoreScoreSpec.hs
│   ├── ObjectsSpec.hs
│   ├── SpecHelpers.hs
│   ├── Spec.hs
│   └── VaisseauSpec.hs
├── package.yaml
├── MegablastXenon2.cabal
└── stack.yaml
```

---

## Organisation MVC

Le projet suit une architecture **Model — View — Controller**.

### Model

Le dossier `src/Model` contient la logique pure du jeu.

Il définit :

- les types du moteur ;
- les hitbox ;
- les vaisseaux ;
- les obstacles ;
- les projectiles ;
- les ennemis ;
- les météores ;
- les murs ;
- les scores ;
- les transitions de fin de tour ;
- les invariants, préconditions et postconditions.

Le modèle ne dépend pas de Gloss pour son évolution logique. Il représente l'état du jeu et les fonctions qui transforment cet état.

### View

Le dossier `src/View` contient le rendu graphique.

Il transforme l'état du modèle en valeurs `Picture` de Gloss :

- écran d'accueil ;
- fond étoilé ;
- murs ;
- vaisseaux ;
- ennemis ;
- projectiles ;
- HUD ;
- écran Game Over.

La vue ne décide pas de la logique métier. Elle lit l'état courant et produit une représentation graphique.

### Controller

Le dossier `src/Controller` fait le lien entre les entrées utilisateur, la boucle Gloss et le moteur.

Il gère :

- les écrans applicatifs (`EcranAccueil`, `Partie`, `EcranGameOver`) ;
- les touches pressées ;
- les tirs en attente ;
- la pause ;
- la réinitialisation ;
- l'accumulation temporelle ;
- l'application des commandes au moteur.

---

## Modules importants

### `app/Main.hs`

Point d'entrée du programme. Il charge les assets et lance `play` de Gloss.

### `Model.Engine.hs`

Façade publique du moteur.

Ce module réexporte les sous-modules de `Model.Engine.*` afin que le reste du programme puisse importer simplement :

```haskell
import Model.Engine
```

Cela évite de propager la complexité interne du moteur dans le contrôleur et dans la vue.

### `Model.Engine.Types.hs`

Définit les types centraux :

- `Script` ;
- `Evenement` ;
- `EvenementPlanifie` ;
- `MursNiveau` ;
- `Moteur`.

Le type `Script` possède des instances `Functor`, `Applicative`, `Monad`, `Foldable`, `Traversable`, `Semigroup` et `Monoid`. Il sert à composer des scénarios et des événements planifiés de manière fonctionnelle.

### `Model.Engine.Step.hs`

Contient la transition principale du moteur.

La fonction importante est :

```haskell
finDeTourMoteurEither :: Moteur -> Either Text Moteur
```

Elle applique les événements planifiés, génère les objets dynamiques, déplace les obstacles, projectiles, ennemis et météores, résout les collisions, met à jour le tour et préserve les invariants du moteur.

### `Model.Engine.Collisions.hs`

Gère la résolution des collisions.

Le module conserve des **slots stables** pour les joueuses : une joueuse éliminée n'est pas supprimée physiquement de la liste. Cela permet de préserver l'association entre joueur, score et commandes.

### `Model.Engine.Murs.hs`

Gère les murs du niveau.

Une difficulté importante vient du fait que les murs peuvent être définis comme des structures longues ou potentiellement infinies. Pour éviter les ralentissements, le moteur ne traite que la partie utile et visible du mur.

### `Controller.Controller.hs`

Gère les entrées clavier et l'avancement de la simulation.

Le contrôleur distingue :

- les touches maintenues pour les déplacements ;
- les tirs ponctuels issus de `EventKey Down`.

Les tirs sont stockés dans `asTirsEnAttente`, consommés une seule fois au prochain tour moteur, puis effacés. Cette stratégie évite qu'un seul appui sur `Entrée` ou `Espace` crée un flux incohérent de projectiles.

### `test/SpecHelpers.hs`

Contient les générateurs QuickCheck du projet.

Les générateurs sont construits pour produire directement des valeurs valides, au lieu de générer massivement des valeurs invalides ensuite filtrées par `suchThat`. Cette approche améliore la stabilité et les performances des tests génératifs.

---

## Tests et propriétés

Le projet suit une démarche de programmation sûre.

Les contrats sont écrits directement en Haskell sous forme de propriétés :

```haskell
prop_inv_...   -- invariant
prop_pre_...   -- précondition
prop_post_...  -- postcondition
```

Les tests couvrent notamment :

- les lois algébriques de `Script`, `Score` et `PV` ;
- les invariants des hitbox ;
- les constructeurs intelligents ;
- les collisions ;
- les murs ;
- les transitions de fin de tour ;
- les slots stables des joueuses ;
- les projectiles ;
- les scores ;
- les générateurs QuickCheck ;
- les états invalides rejetés par les constructeurs.

La suite de tests peut être lancée par :

```bash
stack test
```

---

## Bugs corrigés et choix techniques

### Fenêtrage des murs

Une première version manipulait une trop grande portion des murs. Après plusieurs milliers de tours, le jeu devenait très lent, car les opérations de translation et de collision se faisaient sur une structure trop grande.

La correction consiste à ne conserver et traiter que la partie utile du mur : la portion visible à l'écran, éventuellement augmentée d'une marge de sécurité.

Effet attendu :

- complexité bornée par la fenêtre visible ;
- plus de croissance incontrôlée du coût de rendu ;
- disparition des saccades après plusieurs milliers de tours.

### Tirs multiples sur un seul appui

Un second problème concernait les tirs déclenchés par `Entrée` ou `Espace`. Si le tir était traité comme une touche maintenue, le moteur pouvait créer plusieurs projectiles pour un seul appui, ce qui produisait un flux de tir incohérent.

La correction repose sur une séparation entre :

- les actions continues, comme les déplacements ;
- les actions ponctuelles, comme le tir.

Les tirs sont placés dans `asTirsEnAttente`, appliqués uniquement au premier tour moteur disponible, puis supprimés.

---

## Dépendances

Les dépendances principales sont déclarées dans `package.yaml` :

- `base` ;
- `text` ;
- `containers` ;
- `random` ;
- `gloss` ;
- `JuicyPixels` ;
- `vector` ;
- `hspec` ;
- `QuickCheck`.

---

## Dépannage

### Le jeu ne trouve pas les images

Vérifier que le dossier `assets/` existe à la racine du projet et que le jeu est lancé depuis cette racine :

```bash
pwd
ls assets
stack run MegablastXenon2-exe
```

### Erreur de GHC avec Stack

La configuration actuelle utilise le GHC système.

Vérifier :

```bash
ghc --version
stack --version
```

Si nécessaire, autoriser Stack à installer automatiquement GHC dans `stack.yaml` :

```yaml
install-ghc: true
```

### Problème d'affichage Gloss sous Linux

Installer les bibliothèques graphiques OpenGL/GLUT si elles manquent :

```bash
sudo apt update
sudo apt install freeglut3-dev libgl1-mesa-dev libglu1-mesa-dev
```

---

## Licence

Le projet est distribué sous licence **BSD-3-Clause**, conformément au fichier `LICENSE`.
