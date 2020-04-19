{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Web.OpenWeatherMap.Types.Forecast
  ( Forecast(..)
  ) where

import GHC.Generics (Generic)

import Data.Aeson (FromJSON)

import Web.OpenWeatherMap.Types.Clouds (Clouds)
import Web.OpenWeatherMap.Types.Main (Main)
import Web.OpenWeatherMap.Types.Weather (Weather)
import Web.OpenWeatherMap.Types.Wind (Wind)

data Forecast = Forecast
  { dt :: Int
  , clouds :: Clouds
  , main :: Main
  , weather :: [Weather]
  , wind :: Wind
  } deriving (Show, Generic, FromJSON)
