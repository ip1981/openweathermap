{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Web.OpenWeatherMap.Types.Weather
  ( Weather(..)
  ) where

import GHC.Generics (Generic)

import Data.Aeson (FromJSON)

data Weather = Weather
  { id :: Int
  , main :: String
  , description :: String
  , icon :: String
  } deriving (Show, Generic, FromJSON)
