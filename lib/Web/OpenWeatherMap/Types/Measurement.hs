{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.OpenWeatherMap.Types.Measurement (
  Measurement (..)
) where

import GHC.Generics (Generic)
import Data.Aeson 
import Web.OpenWeatherMap.Types.Main
import Web.OpenWeatherMap.Types.Weather
import Data.Time
import Data.Text

data Measurement = Measurement {
  main :: Main,
  weather :: [Weather],
  dt_txt :: UTCTime
} deriving (Show, Generic)

instance FromJSON Measurement where
  parseJSON = 
    withObject "Measurement" $ \v -> do
      result <- v .: "dt_txt"
      Measurement 
          <$> v .: "main"
          <*> v .: "weather"
          <*> (parseTimeM True defaultTimeLocale "%Y-%-m-%-d %H:%M:%S" result)
