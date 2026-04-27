module View.Assets
  ( AssetsView(..)
  , chargerAssetsView
  ) where

import Codec.Picture
import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap
import qualified Data.Vector.Storable as VS

data AssetsView = AssetsView
  { assetMurGauche :: Maybe Picture
  , assetMurDroit  :: Maybe Picture
  }

chargerAssetsView :: IO AssetsView
chargerAssetsView = do
  murG <- chargerPNG "assets/wall_left_112x1792.png"
  murD <- chargerPNG "assets/wall_right_112x1792.png"
  pure AssetsView
    { assetMurGauche = murG
    , assetMurDroit  = murD
    }

chargerPNG :: FilePath -> IO (Maybe Picture)
chargerPNG chemin = do
  imageE <- readImage chemin
  case imageE of
    Left err -> do
      putStrLn ("Impossible de charger " ++ chemin ++ " : " ++ err)
      pure Nothing

    Right imageDyn -> do
      let image = convertRGBA8 imageDyn
          largeur = imageWidth image
          hauteur = imageHeight image
          donnees = imageData image
          (ptr, _, _) = VS.unsafeToForeignPtr donnees

      pure $
        Just $
          bitmapOfForeignPtr
            largeur
            hauteur
            (BitmapFormat TopToBottom PxRGBA)
            ptr
            True