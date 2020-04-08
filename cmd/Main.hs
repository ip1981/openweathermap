{-# LANGUAGE LambdaCase #-}

module Main
  ( main
  ) where

import           Servant.Client                          (runClientM)
import Web.OpenWeatherMap.API 
import Control.Monad (when, void)
import Data.List (intercalate)
import Data.Semigroup ((<>))
import Data.Version (showVersion)
import System.Exit (die)
import System.IO (IOMode(ReadMode), hGetLine, hPrint, stderr, withFile)

import Options.Applicative
  ( Parser
  , (<**>)
  , (<|>)
  , auto
  , execParser
  , fullDesc
  , header
  , help
  , helper
  , info
  , long
  , metavar
  , option
  , optional
  , short
  , strOption
  , switch
  )
import System.Directory (createDirectoryIfMissing)
import System.Environment.XDG.BaseDir (getUserConfigDir, getUserConfigFile)

import Paths_openweathermap (version) -- from cabal
import qualified Web.OpenWeatherMap.Client as Client
import qualified Web.OpenWeatherMap.Types.Coord as Coord
import qualified Web.OpenWeatherMap.Types.CurrentWeather as CurrentWeather
import qualified Web.OpenWeatherMap.Types.Main as Main
import qualified Web.OpenWeatherMap.Types.Sys as Sys
import qualified Web.OpenWeatherMap.Types.Weather as Weather
import qualified Web.OpenWeatherMap.Types.Wind as Wind

appName :: String
appName = "openweathermap"

parseAction :: Parser (Either Client.Location String)
parseAction = (Left <$> parseLocation ) <|> (Right <$> strOption
      (  long "city-forecast"
      <> short 'f'
      <> metavar "CITY-FORECAST"
      <> help "City name forecast" ))
  
parseLocation :: Parser Client.Location
parseLocation = byName <|> byCoord
  where
    byName = Client.Name <$> strOption
      (  long "city"
      <> short 'c'
      <> metavar "CITY"
      <> help "City name" )
    byCoord = Client.Coord
      <$> option auto
        (  long "lat"
        <> metavar "NUM"
        <> help "Latitude in decimal degrees" )
      <*> option auto
        (  long "lon"
        <> metavar "NUM"
        <> help "Longitude in decimal degrees" )


data ApiKey
  = ApiKeyFile FilePath
  | ApiKey String

parseApiKey :: Parser ApiKey
parseApiKey = fromFile <|> inCmdLine
  where
    fromFile = ApiKeyFile <$> strOption
      (  long "api-key-file"
      <> short 'K'
      <> metavar "APIKEYFILE"
      <> help "Read API key from this file" )

    inCmdLine = ApiKey <$> strOption
      (  long "api-key"
      <> short 'k'
      <> metavar "APIKEY"
      <> help "API key" )

data Config = Config
  { apikey :: Maybe ApiKey
  , location :: Either Client.Location String
  , debug :: Bool
  }

parseConfig :: Parser Config
parseConfig = Config
          <$> optional parseApiKey
          <*> parseAction 
          <*> switch (long "debug" <> short 'd' <> help "Enable debug")

getApiKey :: Maybe ApiKey -> IO String
getApiKey (Just (ApiKey key)) = return key
getApiKey (Just (ApiKeyFile f)) = withFile f ReadMode hGetLine
getApiKey Nothing = do
  createDirectoryIfMissing True =<< getUserConfigDir appName
  getUserConfigFile appName "key" >>= getApiKey . Just . ApiKeyFile

showLocation :: CurrentWeather.CurrentWeather -> String
showLocation w = city ++ maybe "" ("," ++) country ++ " " ++ coords
  where
    name = CurrentWeather.name w
    coord = CurrentWeather.coord w
    country = Sys.country . CurrentWeather.sys $ w
    city =
      if name /= ""
        then name
        else "<unknown>"
    coords =
      "(" ++ show (Coord.lat coord) ++ "°, " ++ show (Coord.lon coord) ++ "°)"

showWeather :: [Weather.Weather] -> String
showWeather w = intercalate "," $ Weather.main <$> w

showHumidity :: Main.Main -> String
showHumidity m = "H " ++ show hm ++ " %"
  where
    hm :: Int
    hm = round . Main.humidity $ m

-- https://en.wikipedia.org/wiki/Millimeter_of_mercury
showPressure :: Main.Main -> String
showPressure m = "P " ++ show p ++ " mmHg"
  where
    hPa2mmHg hpa = hpa * 0.750061561303
    p :: Int
    p = round . hPa2mmHg . Main.pressure $ m

-- https://stackoverflow.com/q/7490660/933161
showWind :: Wind.Wind -> String
showWind w = dir ++ " " ++ show speed ++ " m/s"
  where
    speed :: Int
    speed = round . Wind.speed $ w
    deg = Wind.deg w
    --     [ "N", "NE", "E", "SE", "S", "SW", "W", "NW" ]
    dirs = ["↓", "↙", "←", "↖", "↑", "↗", "→", "↘"]
    l = length dirs
    sector = round $ (deg * fromIntegral l) / 360.0
    dir = dirs !! (sector `rem` l)

showTemp :: Main.Main -> String
showTemp m = "T " ++ temp ++ " °C"
  where
    k2c k = k - 273.15 -- Kelvin to Celsius
    tmax :: Int
    tmin :: Int
    tmax = round . k2c . Main.temp_max $ m
    tmin = round . k2c . Main.temp_min $ m
    show' t =
      if t > 0
        then "+" ++ show t
        else show t
    temp =
      if tmax /= tmin
        then show' tmin ++ ".." ++ show' tmax
        else show' tmin

printWeather :: CurrentWeather.CurrentWeather -> IO ()
printWeather w = putStrLn out
  where
    weather = showWeather $ CurrentWeather.weather w
    place = showLocation w
    mainw = CurrentWeather.main w
    wind = CurrentWeather.wind w
    out =
      place ++
      ": " ++
      intercalate
        ",  "
        [ weather
        , showHumidity mainw
        , showPressure mainw
        , showTemp mainw
        , showWind wind
        ]

run :: Config -> IO ()
run cfg = do
  appid <- getApiKey . apikey $ cfg
  case (location cfg) of
    Left location -> Client.getWeather appid location >>= \case
        Left err      -> die $ show err
        Right weather -> do
          when (debug cfg) $ hPrint stderr weather
          printWeather weather
    Right y -> print =<< runClientM (dailyWeatherForecast (Just y) (Just appid))  =<< Client.defaultEnv

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (parseConfig <**> helper) (fullDesc <> header desc)
    desc =
      "openweathermap " ++
      showVersion version ++
      " - command-line client for https://openweathermap.org/api"
