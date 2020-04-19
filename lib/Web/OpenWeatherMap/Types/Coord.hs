{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Web.OpenWeatherMap.Types.Coord
  ( Coord(..)
  ) where

import GHC.Generics (Generic)

import Data.Aeson (FromJSON)

data Coord = Coord
  { lon :: Maybe Double
  , lat :: Maybe Double
  } deriving (Show, Generic, FromJSON)
