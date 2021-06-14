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



