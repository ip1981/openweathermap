{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Web.OpenWeatherMap.Types.CurrentWeather
  ( CurrentWeather(..)
  ) where

import GHC.Generics (Generic)
import Prelude hiding (id)

import Data.Aeson (FromJSON)

import Web.OpenWeatherMap.Types.Clouds (Clouds)
import Web.OpenWeatherMap.Types.Coord (Coord)
import Web.OpenWeatherMap.Types.Main (Main)
import Web.OpenWeatherMap.Types.Sys (Sys)
import Web.OpenWeatherMap.Types.Weather (Weather)
import Web.OpenWeatherMap.Types.Wind (Wind)

-- | Response to requests for current weather.
-- Refer to <https://openweathermap.org/current>.
data CurrentWeather = CurrentWeather
  { coord :: Coord
  , weather :: [Weather]
  , base :: String
  , main :: Main
  , wind :: Wind
  , clouds :: Clouds
  , dt :: Int
  , sys :: Sys
  , id :: Int
  , name :: String
  , cod :: Int
  } deriving (Show, Generic, FromJSON)
