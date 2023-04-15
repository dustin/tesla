# tesla

Tesla API for Haskell.

See also: https://tesla-api.timdorr.com/

## Authentication

This library used to support authenticating via username and password to get credentials, but Tesla busted that pretty hard.

The best practice at this point is to use something like https://www.teslafi.com/tokenUser.php to get a bearer and refresh token and feed them to the library via `fromToken` and `refreshAuth`