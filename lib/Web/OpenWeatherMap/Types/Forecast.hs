{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Web.OpenWeatherMap.Types.Forecast (
  Forecast (..)
) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON)
import Web.OpenWeatherMap.Types.Measurement

data Forecast = Forecast 
  { cod :: String
  , message :: Int
  , cnt :: Int
  , list :: [Measurement]
  -- , city
  } deriving (Show, Generic, FromJSON)
