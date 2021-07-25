#!/usr/bin/env bash

export DEVELOPMENT=True

stack install pact-web-server \
  --file-watch \
  --no-nix-pure \
  --exec='./scripts/restart-pact-web-server.sh'

