{-|
Direct API functions.
For API key (a.k.a appid) refer to <http://openweathermap.org/appid>.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Web.OpenWeatherMap.API
  ( weatherByName
  , weatherByCoord
  , forecastByName
  , forecastByCoord
  ) where

import Data.Proxy (Proxy(..))

import Servant.API ((:<|>)(..), (:>), Get, JSON, QueryParam)
import Servant.Client (ClientM, client)

import Web.OpenWeatherMap.Types.CurrentWeather (CurrentWeather)
import Web.OpenWeatherMap.Types.ForecastWeather (ForecastWeather)

type GetCurrentWeather = AppId :> Get '[ JSON] CurrentWeather

type GetForecastWeather = AppId :> Get '[ JSON] ForecastWeather

type AppId = QueryParam "appid" String

type Current
   = "weather" :> (QueryParam "q" String :> GetCurrentWeather :<|> QueryParam "lat" Double :> QueryParam "lon" Double :> GetCurrentWeather)

type Forecast
   = "forecast" :> (QueryParam "q" String :> GetForecastWeather :<|> QueryParam "lat" Double :> QueryParam "lon" Double :> GetForecastWeather)

type API = Current :<|> Forecast

-- | Request current weather in the city.
weatherByName ::
     Maybe String -- ^ City name, e. g. \"Moscow\" or \"Moscow,ru\".
  -> Maybe String -- ^ API key.
  -> ClientM CurrentWeather
-- | Request current weather at the geographic coordinates (in decimal degrees).
weatherByCoord ::
     Maybe Double -- ^ Latitude, e. g. 55.7522200 for Moscow.
  -> Maybe Double -- ^ Longitude, e. g. 37.6155600 for Moscow.
  -> Maybe String -- ^ API key.
  -> ClientM CurrentWeather
-- | Request forecast weather in the city.
forecastByName ::
     Maybe String -- ^ City name, e. g. \"Moscow\" or \"Moscow,ru\".
  -> Maybe String -- ^ API key.
  -> ClientM ForecastWeather
-- | Request current weather at the geographic coordinates (in decimal degrees).
forecastByCoord ::
     Maybe Double -- ^ Latitude, e. g. 55.7522200 for Moscow.
  -> Maybe Double -- ^ Longitude, e. g. 37.6155600 for Moscow.
  -> Maybe String -- ^ API key.
  -> ClientM ForecastWeather
(weatherByName :<|> weatherByCoord) :<|> (forecastByName :<|> forecastByCoord) =
  client (Proxy :: Proxy API)
