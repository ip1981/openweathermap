module Print
  ( printCurrectWeather
  , printForecastWeather
  ) where

import Data.List (intercalate)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.LocalTime (TimeZone, minutesToTimeZone, utcToZonedTime)
import Text.Printf (printf)

import Web.OpenWeatherMap.Formulas (absoluteHumidity)
import qualified Web.OpenWeatherMap.Types.City as City
import qualified Web.OpenWeatherMap.Types.Coord as Coord
import qualified Web.OpenWeatherMap.Types.CurrentWeather as CW
import qualified Web.OpenWeatherMap.Types.Forecast as FC
import qualified Web.OpenWeatherMap.Types.ForecastWeather as FW
import qualified Web.OpenWeatherMap.Types.Main as Main
import qualified Web.OpenWeatherMap.Types.Sys as Sys
import qualified Web.OpenWeatherMap.Types.Weather as Weather
import qualified Web.OpenWeatherMap.Types.Wind as Wind

printCurrectWeather :: CW.CurrentWeather -> IO ()
printCurrectWeather cw =
  putStrLn
    (place ++
     ", " ++
     time ++
     ": " ++
     intercalate
       ",  "
       [ w
       , showHumidity mainw
       , showAbsoluteHumidity mainw
       , showPressure mainw
       , showTemp mainw
       , showWind wind
       ])
  where
    tz = secondsToTimeZone (CW.timezone cw)
    time = showLocalTime tz (CW.dt cw)
    w = showWeather $ CW.weather cw
    place = showLocation (CW.name cw) (Sys.country . CW.sys $ cw) (CW.coord cw)
    mainw = CW.main cw
    wind = CW.wind cw

printForecastWeather :: FW.ForecastWeather -> IO ()
printForecastWeather fw = do
  let c = FW.city fw
      tz = secondsToTimeZone (City.timezone c)
      place = showLocation (City.name c) (City.country c) (City.coord c)
  putStrLn place
  mapM_ putStrLn (showForecast tz <$> FW.list fw)

secondsToTimeZone :: Int -> TimeZone
secondsToTimeZone s = minutesToTimeZone (s `div` 60)

showForecast :: TimeZone -> FC.Forecast -> String
showForecast tz fc =
  showLocalTime tz (FC.dt fc) ++
  ": " ++
  intercalate
    ", "
    [ showWeather (FC.weather fc)
    , showHumidity mainw
    , showAbsoluteHumidity mainw
    , showPressure mainw
    , showTemp mainw
    , showWind (FC.wind fc)
    ]
  where
    mainw = FC.main fc

showLocalTime :: TimeZone -> Int -> String
showLocalTime tz =
  show . utcToZonedTime tz . posixSecondsToUTCTime . fromIntegral

showLocation :: String -> Maybe String -> Coord.Coord -> String
showLocation name country coord =
  name' ++ maybe "" ("," ++) country ++ " " ++ coords
  where
    coords = showCoord coord
    name' =
      if name /= ""
        then name
        else "<unknown>"

showCoord :: Coord.Coord -> String
showCoord coord =
  "(" ++
  maybe "?" show (Coord.lat coord) ++
  "°, " ++ maybe "?" show (Coord.lon coord) ++ "°)"

showWeather :: [Weather.Weather] -> String
showWeather w = intercalate "/" $ Weather.main <$> w

showHumidity :: Main.Main -> String
showHumidity m = "H " ++ show hm ++ " %"
  where
    hm :: Int
    hm = round . Main.humidity $ m

showAbsoluteHumidity :: Main.Main -> String
showAbsoluteHumidity m = "ρ " ++ rho ++ " g/m³"
  where
    r = absoluteHumidity m
    rho = maybe "??" (printf "%0.2f") r

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
