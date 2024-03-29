{-|
High-level client functions perfoming requests to OpenWeatherMap API.
-}
module Web.OpenWeatherMap.Client
  ( getWeather
  , getForecast
  ) where

import Network.HTTP.Client.TLS (newTlsManager)
import Servant.Client
  ( BaseUrl(BaseUrl)
  , ClientEnv
  , ClientError
  , Scheme(Https)
  , mkClientEnv
  , runClientM
  )

import qualified Web.OpenWeatherMap.API as API
import Web.OpenWeatherMap.Types.CurrentWeather (CurrentWeather)
import Web.OpenWeatherMap.Types.ForecastWeather (ForecastWeather)
import Web.OpenWeatherMap.Types.Location (Location)

-- | Make a request to OpenWeatherMap API
--   and return current weather in given location.
getWeather ::
     String -- ^ API key.
  -> Location
  -> IO (Either ClientError CurrentWeather)
getWeather appid loc = defaultEnv >>= runClientM (API.currentWeather appid loc)

-- | Make a request to OpenWeatherMap API
--   and return forecast weather in given location.
getForecast ::
     String -- ^ API key.
  -> Location
  -> IO (Either ClientError ForecastWeather)
getForecast appid loc =
  defaultEnv >>= runClientM (API.forecastWeather appid loc)

defaultEnv :: IO ClientEnv
defaultEnv = do
  manager <- newTlsManager
  return $ mkClientEnv manager baseUrl

baseUrl :: BaseUrl
baseUrl = BaseUrl Https "api.openweathermap.org" 443 "/data/2.5"
