{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Web.OpenWeatherMap.Types.Location
  ( Location(..)
  ) where

import Data.ByteString (ByteString)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Servant.API ((:>))
import Servant.Client (Client, HasClient, clientWithRoute, hoistClientMonad)
import Servant.Client.Core.Request (Request, appendToQueryString)
import Web.HttpApiData (ToHttpApiData, toQueryParam)

-- | Various ways to specify location.
data Location
  = Name String -- ^ City name.
  | Coord Double Double -- ^ Geographic coordinates: latitude and longitude.

addParam :: ToHttpApiData a => Text -> a -> Request -> Request
addParam name = appendToQueryString name . Just . encodeUtf8 . toQueryParam

instance HasClient m api => HasClient m (Location :> api) where
  type Client m (Location :> api) = Location -> Client m api
  hoistClientMonad pm _ f cl = hoistClientMonad pm (Proxy :: Proxy api) f . cl
  clientWithRoute pm Proxy req loc =
    clientWithRoute pm (Proxy :: Proxy api) (addParams loc req)
    where
      addParams (Name q) = addParam "q" q
      addParams (Coord lat lon) = addParam "lat" lat . addParam "lon" lon
