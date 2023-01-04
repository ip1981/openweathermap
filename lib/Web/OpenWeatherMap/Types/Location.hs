{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Web.OpenWeatherMap.Types.Location
  ( Location(..)
  ) where

import Data.Proxy (Proxy(..))

import Servant.API ((:>))
import Servant.Client (Client, HasClient, clientWithRoute, hoistClientMonad)
import Servant.Client.Core.Request (appendToQueryString)
import Web.HttpApiData (toQueryParam)
import Data.Text.Encoding (encodeUtf8)

-- | Various ways to specify location.
data Location
  = Name String -- ^ City name.
  | Coord Double Double -- ^ Geographic coordinates: latitude and longitude.

instance HasClient m api => HasClient m (Location :> api) where
  type Client m (Location :> api) = Location -> Client m api
  clientWithRoute pm Proxy req loc =
    clientWithRoute pm (Proxy :: Proxy api) (addParams loc req)
    where
      addParams (Name q) = appendToQueryString "q" $ (Just . encodeUtf8 . toQueryParam) q
      addParams (Coord lat lon) =
        appendToQueryString "lat" ((Just . encodeUtf8 . toQueryParam) lat) .
        appendToQueryString "lon" ((Just . encodeUtf8 . toQueryParam) lon)
  hoistClientMonad pm _ f cl = hoistClientMonad pm (Proxy :: Proxy api) f . cl
