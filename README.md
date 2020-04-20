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
Norilsk,RU (69.35°, 88.2°): Clouds,  H 100 %,  P 753 mmHg,  T +4 °C,  ↓ 1 m/s

$ openweathermap --lat 55.7522200 --lon 37.6155600
Moscow,RU (55.75°, 37.62°): Clear,  H 45 %,  P 762 mmHg,  T +18..+21 °C,  → 4 m/s

$ openweathermap -q kaliningrad -f
Kaliningrad,RU (54.7065°, 20.511°)
2020-04-19 17:00:00 +0200: Clear, H 66 %, P 767 mmHg, T +7 °C, ↓ 7 m/s
2020-04-19 20:00:00 +0200: Clear, H 79 %, P 768 mmHg, T +4 °C, ↓ 5 m/s
2020-04-19 23:00:00 +0200: Clear, H 84 %, P 769 mmHg, T +3 °C, ↓ 4 m/s
2020-04-20 02:00:00 +0200: Clear, H 85 %, P 770 mmHg, T +3 °C, ↓ 3 m/s
2020-04-20 05:00:00 +0200: Clear, H 84 %, P 770 mmHg, T +2 °C, ↓ 3 m/s
2020-04-20 08:00:00 +0200: Clear, H 76 %, P 770 mmHg, T +5 °C, ↓ 4 m/s
2020-04-20 11:00:00 +0200: Clear, H 65 %, P 771 mmHg, T +8 °C, ↓ 4 m/s
2020-04-20 14:00:00 +0200: Clear, H 62 %, P 771 mmHg, T +9 °C, ↓ 4 m/s
...

$ openweathermap --lat -12.0432 --lon -77.0282 -f
Lima,PE (-12.0432°, -77.0282°)
2020-04-19 10:00:00 -0500: Clear, H 70 %, P 764 mmHg, T +22..+24 °C, ↗ 3 m/s
2020-04-19 13:00:00 -0500: Clear, H 64 %, P 762 mmHg, T +23..+25 °C, ↗ 4 m/s
2020-04-19 16:00:00 -0500: Clouds, H 66 %, P 761 mmHg, T +23..+24 °C, ↑ 4 m/s
2020-04-19 19:00:00 -0500: Clouds, H 72 %, P 763 mmHg, T +22 °C, ↑ 4 m/s
2020-04-19 22:00:00 -0500: Clouds, H 72 %, P 764 mmHg, T +22 °C, ↑ 3 m/s
2020-04-20 01:00:00 -0500: Clouds, H 76 %, P 763 mmHg, T +21 °C, ↑ 2 m/s
2020-04-20 04:00:00 -0500: Clear, H 80 %, P 763 mmHg, T +20 °C, ↑ 3 m/s
2020-04-20 07:00:00 -0500: Clouds, H 79 %, P 764 mmHg, T +20 °C, ↑ 3 m/s
2020-04-20 10:00:00 -0500: Clouds, H 72 %, P 764 mmHg, T +21 °C, ↗ 3 m/s
...
```



