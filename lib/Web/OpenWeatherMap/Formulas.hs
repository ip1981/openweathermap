{-# LANGUAGE NamedFieldPuns #-}

module Web.OpenWeatherMap.Formulas
  ( absoluteHumidity
  ) where

import Web.OpenWeatherMap.Types.Main

{-- | Calculate absolute humidity (g/m³)

Returns 'Nothing' if the temperature is out of range (−30°C,  +35°C).

-}
absoluteHumidity :: Main -> Maybe Double
absoluteHumidity Main {temp, humidity}
  | tC > -30 && tC < 35 = Just $ ahBolton1980 temp humidity
  | otherwise = Nothing
  where
    tC = k2c temp

{-- | Calculate absolute humidity (g/m³)

Ref.: Bolton D. "The Computation of Equivalent Potential Temperature",
Monthly Weather Review, 1980, 108(7):1046–1053.

-}
ahBolton1980 ::
     Double -- ^ Temperature in Kelvins
  -> Double -- ^ Relative humidity in %
  -> Double
ahBolton1980 temp humidity =
  13.25 * humidity * exp (17.67 * tC / (tC + 243.5)) / temp
  where
    tC = k2c temp

k2c :: Double -> Double
k2c t = t - 273.15
