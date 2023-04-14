module Text.Shows where

import Prelude

class Shows a where
  shows :: a -> String -> String

newtype ViaShows a
  = ViaShows a

instance showShows :: Shows a => Show (ViaShows a) where
  show (ViaShows a) = shows a ""
