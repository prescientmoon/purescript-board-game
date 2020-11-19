module GameMap where

newtype BackgroundMap
  = BackgroundMap
  { width :: Int
  , height :: Int
  , url :: String
  }
