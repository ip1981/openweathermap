{-|
High-level client functions perfoming requests to OpenWeatherMap API.
-}
module Web.OpenWeatherMap.Client
  ( Location(..)
  , getWeather
  ) where

import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.Client
  ( BaseUrl(BaseUrl)
  , ClientEnv
  , ClientError
  , ClientM
  , Scheme(Http)
  , mkClientEnv
  , runClientM
  )

import qualified Web.OpenWeatherMap.API as API
import Web.OpenWeatherMap.Types.CurrentWeather (CurrentWeather)

-- | Various way to specify location.
data Location
  = Name String -- ^ City name.
  | Coord Double
          Double -- ^ Geographic coordinates: latitude and longitude.

-- | Make a request to OpenWeatherMap API
--   and return current weather in given location.
getWeather ::
     String -- ^ API key.
  -> Location
  -> IO (Either ClientError CurrentWeather)
getWeather appid loc = defaultEnv >>= runClientM (api loc appid)

api ::
     Location
  -> String -- ^ API key.
  -> ClientM CurrentWeather
api (Name city) = API.weatherByName (Just city) . Just
api (Coord lat lon) = API.weatherByCoord (Just lat) (Just lon) . Just

defaultEnv :: IO ClientEnv
defaultEnv = do
  manager <- newManager defaultManagerSettings
  return $ mkClientEnv manager baseUrl

-- XXX openweathermap.org does not support HTTPS,
-- XXX appid is passed in clear text. Oops.
baseUrl :: BaseUrl
baseUrl = BaseUrl Http "api.openweathermap.org" 80 "/data/2.5"
