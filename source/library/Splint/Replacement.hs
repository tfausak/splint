module Splint.Replacement where

import qualified GHC.Plugins as Plugin

newtype Replacement
  = Replacement String
  deriving (Eq, Show)

instance Plugin.Outputable Replacement where
  ppr = Plugin.text . toString

fromString :: String -> Replacement
fromString = Replacement

toString :: Replacement -> String
toString (Replacement x) = x
