# Changelog for tesla

# Release 0.7.1.0

Updated some available commands.

# Release 0.7.0.0

Authentication is no longer supported.

It hasn't worked for about a year now and Tesla aggressively thwarts automation.

# Release 0.6.0.0

All APIs that send data use JSON now.

Previously we sent form/data and that was fine, but apparently no longer is.

# Release 0.5.0.0

Added:

1. maxDefrost
2. bioweapon defense mode controls
3. setAmps
4. scheduled charging configuration
5. scheduled departure configuration

Also added a prism for viewing `VehicleData` as an Aeson `Value`.

# Release 0.3.1.0

Authentication is supported again (though not yet MFA path).

More access to more APIs where available.

# Release 0.1.0.1

A more informative exception is thrown from `runNamedCar` when an
unknown car name is given.

## Unreleased changes
