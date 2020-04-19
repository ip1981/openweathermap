{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Web.OpenWeatherMap.Types.City
  ( City(..)
  ) where

import GHC.Generics (Generic)

import Data.Aeson (FromJSON)

import Web.OpenWeatherMap.Types.Coord (Coord)

data City = City
  { name :: String
  , country :: Maybe String
  , coord :: Coord
  , timezone :: Int
  , sunset :: Int
  , sunrise :: Int
  } deriving (Show, Generic, FromJSON)
