# PACT codebase

Development:
- `nix develop` to set up a development environment, where all necessary
  packages are available
- For backend development
  * `stack build --file-watch`
  * `stack test --file-watch --ghc-options="-freverse-errors -j4 +RTS -A128M
    -n2m -RTS`
- For frontend development/design: `./scripts/devel-pact-web-server.sh`
- Set up local hoogle server: `hoogle serve --local`, then check
  `localhost:8080`
- Generate test coverage report: `stack test --enable-coverage`



