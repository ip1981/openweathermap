0.3.0
=====

  * Switch from HTTP to HTTPS API (https://api.openweathermap.org/data/2.5).
  * Command line utility prints absolute humidity and local time.
  * Require Servant >= 0.19.


0.2.0
=====

  * (* BREAKING *) Made Location a part of the API to
    reduce code duplication.

  * (* BREAKING *) Made all query parameters required.

  * Changed -c to -q for the command line utility.

  * Added forecast weather API.


0.1.0
=====

  * Removed the `message` field from the `Sys` type.
    It was some internal parameter and might be absent.

  * Require servant-client >= 0.16 (for `ClientError` instead of `ServantError`).


0.0.0
=====

  * Initial version.

