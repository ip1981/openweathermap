{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Web.OpenWeatherMap.Types.Clouds
  ( Clouds(..)
  ) where

import GHC.Generics (Generic)

import Data.Aeson (FromJSON)

data Clouds = Clouds
  { all :: Double
  } deriving (Show, Generic, FromJSON)
