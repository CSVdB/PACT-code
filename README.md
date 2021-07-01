# PACT

Main decisions:
- Backend is written in Haskell
  * Formatter: Ormulu
  * Linter: hlint
  * Package management: Nix
- Frontend is written in Purescript
  * Package management: Nix
- CI: Nix-based (github workflows)
- Deployment is done in Nix on an EC2 AWS instance running NixOS

# Deploying the backend server

- Set up a `config.yaml` file in the directory from where you want to deploy
  * See `pact-api-server --help`
  * This should contain the port to set the server up on, the database filepath
    and the log level (see `LogLevel` constructors in `Control.Monad.Logger`)
  * The pre-defined NixOS module already contains the config. `LogLevel` must
    still be added there, though.

# Haskell tips

- If you want to log any information in the server, you can use `logInfoN ::
  Text -> H ()`
  * https://hackage.haskell.org/package/monad-logger-0.3.36/docs/Control-Monad-Logger.html#v:logInfoN
  * Same for `logDebugN` etc.
  * Each API endpoint might want to have its own logging, in the
    `PACT.API.Server.Handler.<SomeHandler>` modules



