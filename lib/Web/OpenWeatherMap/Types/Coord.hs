{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Web.OpenWeatherMap.Types.Coord (
  Coord(..)
) where

import GHC.Generics (Generic)

import Data.Aeson (FromJSON)


data Coord = Coord
  { lon :: Double
  , lat :: Double
  } deriving (Show, Generic, FromJSON)

