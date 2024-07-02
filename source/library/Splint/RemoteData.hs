module Splint.RemoteData where

data RemoteData e a
  = NotAsked
  | Loading
  | Failure e
  | Success a
  deriving (Eq, Show)
