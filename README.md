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
Usage: openweathermap ([-K|--api-key-file APIKEYFILE] | [-k|--api-key APIKEY])
                      ((-c|--city CITY) | --lat NUM --lon NUM) [-d|--debug]

Available options:
  -K,--api-key-file APIKEYFILE
                           Read API key from this file
  -k,--api-key APIKEY      API key
  -c,--city CITY           City name
  --lat NUM                Latitude in decimal degrees
  --lon NUM                Longitude in decimal degrees
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
$ openweathermap -c norilsk
Norilsk,RU (69.35°, 88.2°): Clouds,  H 100 %,  P 753 mmHg,  T +4 °C,  ↓ 1 m/s

$ openweathermap --lat 55.7522200 --lon 37.6155600
Moscow,RU (55.75°, 37.62°): Clear,  H 45 %,  P 762 mmHg,  T +18..+21 °C,  → 4 m/s
```



