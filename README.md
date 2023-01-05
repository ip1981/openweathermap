OpenWeatherMap
==============

OpenWeatherMap is a haskell library to access <https://openweathermap.org/api>.

Requirements
============

OpenWeatherMap is written in Haskell with [GHC](http://www.haskell.org/ghc/).
All required Haskell libraries are listed in [openweathermap.cabal](openweathermap.cabal).
Use [cabal-install](http://www.haskell.org/haskellwiki/Cabal-Install) to fetch
and build all pre-requisites automatically.


Command-line utility
====================

The command-line utility `openweathermap` provides a means
to get human readable weather infromation.

Usage
-----

```
Usage: openweathermap [(-K|--api-key-file APIKEYFILE) | (-k|--api-key APIKEY)]
                      ((-q|--query STRING) | --lat NUM --lon NUM)
                      [(-n|--current) | (-f|--forecast)] [-d|--debug]

Available options:
  -K,--api-key-file APIKEYFILE
                           Read API key from this file
  -k,--api-key APIKEY      API key
  -q,--query STRING        City name, e. g. Santiago or Santiago,CU
  --lat NUM                Latitude in decimal degrees
  --lon NUM                Longitude in decimal degrees
  -n,--current             current weather (default)
  -f,--forecast            forecast weather
  -d,--debug               Enable debug
  -h,--help                Show this help text

```
By default, `openweathermap` reads the API key
from the `$XDG_CONFIG_HOME/openweathermap/key` file, where
[`$XDG_CONFIG_HOME`](https://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html)
is typically `~/.config` on Linux systems.


Examples
--------

```
$ openweathermap -q norilsk
Noril'sk,RU (69.3535°, 88.2027°), 2023-01-05 19:02:05 +0700: Clouds,  H 98 %,  ρ ?? g/m³,  P 764 mmHg,  T -35 °C,  → 1 m/s

$ openweathermap --lat 55.7522200 --lon 37.6155600
Moscow,RU (55.7522°, 37.6156°), 2023-01-05 15:02:02 +0300: Clouds,  H 95 %,  ρ 3.05 g/m³,  P 755 mmHg,  T -8..-5 °C,  ↙ 3 m/s

$ openweathermap -q калининград -f
Kaliningrad,RU (54.7065°, 20.511°)
2023-01-05 17:00:00 +0200: Snow, H 93 %, ρ 6.21 g/m³, P 750 mmHg, T +2..+5 °C, ↙ 6 m/s
2023-01-05 20:00:00 +0200: Clouds, H 88 %, ρ 4.82 g/m³, P 755 mmHg, T 0..+2 °C, ↙ 7 m/s
2023-01-05 23:00:00 +0200: Clouds, H 81 %, ρ 3.37 g/m³, P 761 mmHg, T -2 °C, ↙ 5 m/s
2023-01-06 02:00:00 +0200: Clouds, H 73 %, ρ 2.78 g/m³, P 764 mmHg, T -3 °C, ↙ 5 m/s
...


$ openweathermap --lat -12.0432 --lon -77.0282 -f
Lima,PE (-12.0432°, -77.0282°)
2023-01-05 10:00:00 -0500: Clouds, H 79 %, ρ 14.63 g/m³, P 749 mmHg, T +21..+23 °C, ↑ 3 m/s
2023-01-05 13:00:00 -0500: Clouds, H 70 %, ρ 14.35 g/m³, P 755 mmHg, T +23..+24 °C, ↗ 4 m/s
2023-01-05 16:00:00 -0500: Clouds, H 70 %, ρ 14.26 g/m³, P 760 mmHg, T +23 °C, ↑ 5 m/s
2023-01-05 19:00:00 -0500: Clouds, H 75 %, ρ 14.28 g/m³, P 761 mmHg, T +22 °C, ↑ 3 m/s
2023-01-05 22:00:00 -0500: Clouds, H 78 %, ρ 14.55 g/m³, P 762 mmHg, T +21 °C, ↑ 2 m/s
...
```

