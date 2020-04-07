{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Web.OpenWeatherMap.Types.Main
  ( Main(..)
  ) where

import GHC.Generics (Generic)

import Data.Aeson (FromJSON)

{-# ANN module "HLint: ignore Use camelCase" #-}

data Main = Main
  { temp :: Double
  , pressure :: Double
  , humidity :: Double
  , temp_min :: Double
  , temp_max :: Double
  , sea_level :: Maybe Double
  , grnd_level :: Maybe Double
  } deriving (Show, Generic, FromJSON)
