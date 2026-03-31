{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Eta reduce" -}
{- HLINT ignore "Use record patterns" -}
{- HLINT ignore "Use camelCase" -}
{-# LANGUAGE OverloadedStrings #-}


module Lib
  ( 
  -- Utilitaires
   tshow
  ) where

import Data.Text (Text)
import qualified Data.Text as T

tshow :: Show a => a -> Text
tshow = T.pack . show