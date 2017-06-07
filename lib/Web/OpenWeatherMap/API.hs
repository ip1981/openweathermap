{-|
Direct API functions.
For API key (a.k.a appid) refer to <http://openweathermap.org/appid>.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Web.OpenWeatherMap.API (
  weatherByName,
  weatherByCoord
) where

import Data.Proxy (Proxy(..))

import Servant.API ((:>), (:<|>)(..), JSON, Get, QueryParam)
import Servant.Client (client)
import Servant.Common.Req (ClientM)

import Web.OpenWeatherMap.Types.CurrentWeather (CurrentWeather)


type GetCurrentWeather = AppId :> Get '[JSON] CurrentWeather
type AppId = QueryParam "appid" String

type API
     = "weather" :> QueryParam "q" String :> GetCurrentWeather
  :<|> "weather" :> QueryParam "lat" Double :> QueryParam "lon" Double
                 :> GetCurrentWeather

-- | Request current weather in the city.
weatherByName
  :: Maybe String  -- ^ City name, e. g. \"Moscow\" or \"Moscow,ru\".
  -> Maybe String  -- ^ API key.
  -> ClientM CurrentWeather

-- | Request current weather at the geographic coordinates (in decimal degrees).
weatherByCoord
  :: Maybe Double  -- ^ Latitude, e. g. 55.7522200 for Moscow.
  -> Maybe Double  -- ^ Longitude, e. g. 37.6155600 for Moscow.
  -> Maybe String  -- ^ API key.
  -> ClientM CurrentWeather

weatherByName :<|> weatherByCoord = client (Proxy :: Proxy API)

