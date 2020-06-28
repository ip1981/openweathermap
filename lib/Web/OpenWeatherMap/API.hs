{-|
Direct API functions.
For API key (a.k.a appid) refer to <http://openweathermap.org/appid>.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Web.OpenWeatherMap.API
  ( currentWeather
  , forecastWeather
  ) where

import Data.Proxy (Proxy(..))

import Servant.API ((:<|>)(..), (:>), Get, JSON, QueryParam', Required, Strict)
import Servant.Client (ClientM, client)

import Web.OpenWeatherMap.Types.CurrentWeather (CurrentWeather)
import Web.OpenWeatherMap.Types.ForecastWeather (ForecastWeather)
import Web.OpenWeatherMap.Types.Location (Location)

type QueryParam = QueryParam' '[ Required, Strict]

type Current
   = "weather" :> QueryParam "appid" String :> Location :> Get '[ JSON] CurrentWeather

type Forecast
   = "forecast" :> QueryParam "appid" String :> Location :> Get '[ JSON] ForecastWeather

type API = Current :<|> Forecast

forecastWeather :: String -> Location -> ClientM ForecastWeather
currentWeather :: String -> Location -> ClientM CurrentWeather
(currentWeather :<|> forecastWeather) = client (Proxy :: Proxy API)
