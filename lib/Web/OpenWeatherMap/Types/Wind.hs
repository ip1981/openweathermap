{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Web.OpenWeatherMap.Types.Wind
  ( Wind(..)
  ) where

import GHC.Generics (Generic)

import Data.Aeson (FromJSON)

data Wind = Wind
  { speed :: Double
  , deg :: Double
  } deriving (Show, Generic, FromJSON)
