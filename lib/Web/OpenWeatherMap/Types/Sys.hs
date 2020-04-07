{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Web.OpenWeatherMap.Types.Sys
  ( Sys(..)
  ) where

import GHC.Generics (Generic)

import Data.Aeson (FromJSON)

data Sys = Sys
  { country :: Maybe String
  , sunrise :: Int
  , sunset :: Int
  } deriving (Show, Generic, FromJSON)
