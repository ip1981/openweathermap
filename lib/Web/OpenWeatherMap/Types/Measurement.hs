{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Web.OpenWeatherMap.Types.Measurement (
  Measurement (..)
) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON)
import Web.OpenWeatherMap.Types.Main
import Web.OpenWeatherMap.Types.Weather 

data Measurement = Measurement {
  main :: Main,
  weather :: [Weather]
  } deriving (Show, Generic, FromJSON)

