{-# LANGUAGE LambdaCase #-}

module Main
  ( main
  ) where

import Control.Monad (when)
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
  , flag'
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

import qualified Web.OpenWeatherMap.Client as Client
import Web.OpenWeatherMap.Types.Location (Location(..))

import Paths_openweathermap (version) -- from cabal
import Print (printCurrectWeather, printForecastWeather)

appName :: String
appName = "openweathermap"

parseLocation :: Parser Location
parseLocation = byName <|> byCoord
  where
    byName =
      Name <$>
      strOption
        (long "query" <> short 'q' <> metavar "STRING" <>
         help "City name, e. g. Santiago or Santiago,CU")
    byCoord =
      Coord <$>
      option
        auto
        (long "lat" <> metavar "NUM" <> help "Latitude in decimal degrees") <*>
      option
        auto
        (long "lon" <> metavar "NUM" <> help "Longitude in decimal degrees")

data ApiKey
  = ApiKeyFile FilePath
  | ApiKey String

parseApiKey :: Parser ApiKey
parseApiKey = fromFile <|> inCmdLine
  where
    fromFile =
      ApiKeyFile <$>
      strOption
        (long "api-key-file" <> short 'K' <> metavar "APIKEYFILE" <>
         help "Read API key from this file")
    inCmdLine =
      ApiKey <$>
      strOption
        (long "api-key" <> short 'k' <> metavar "APIKEY" <> help "API key")

data Weather
  = Current
  | Forecast

parseWeather :: Parser Weather
parseWeather =
  flag'
    Current
    (long "current" <> short 'n' <> help "current weather (default)") <|>
  flag' Forecast (long "forecast" <> short 'f' <> help "forecast weather") <|>
  pure Current

data Config =
  Config
    { apikey :: Maybe ApiKey
    , location :: Location
    , weather :: Weather
    , debug :: Bool
    }

parseConfig :: Parser Config
parseConfig =
  Config <$> optional parseApiKey <*> parseLocation <*> parseWeather <*>
  switch (long "debug" <> short 'd' <> help "Enable debug")

getApiKey :: Maybe ApiKey -> IO String
getApiKey (Just (ApiKey key)) = return key
getApiKey (Just (ApiKeyFile f)) = withFile f ReadMode hGetLine
getApiKey Nothing = do
  createDirectoryIfMissing True =<< getUserConfigDir appName
  getUserConfigFile appName "key" >>= getApiKey . Just . ApiKeyFile

run :: Config -> IO ()
run cfg = do
  appid <- getApiKey . apikey $ cfg
  case weather cfg of
    Current ->
      Client.getWeather appid (location cfg) >>= \case
        Left err -> die $ show err
        Right cw -> do
          when (debug cfg) $ hPrint stderr cw
          printCurrectWeather cw
    Forecast ->
      Client.getForecast appid (location cfg) >>= \case
        Left err -> die $ show err
        Right fw -> do
          when (debug cfg) $ hPrint stderr fw
          printForecastWeather fw

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (parseConfig <**> helper) (fullDesc <> header desc)
    desc =
      "openweathermap " ++
      showVersion version ++
      " - command-line client for https://openweathermap.org/api"
