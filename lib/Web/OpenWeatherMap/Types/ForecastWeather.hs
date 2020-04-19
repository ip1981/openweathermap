{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Web.OpenWeatherMap.Types.ForecastWeather
  ( ForecastWeather(..)
  ) where

import GHC.Generics (Generic)

import Data.Aeson (FromJSON)

import Web.OpenWeatherMap.Types.City (City)
import Web.OpenWeatherMap.Types.Forecast (Forecast)

-- | Response to requests for forecast weather.
-- Refer to <https://openweathermap.org/forecast5>.
data ForecastWeather = ForecastWeather
  { list :: [Forecast]
  , city :: City
  } deriving (Show, Generic, FromJSON)
