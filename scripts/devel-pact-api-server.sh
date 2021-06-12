#!/usr/bin/env bash

stack install pact-api-server \
  --file-watch \
  --exec='./scripts/restart-pact-api-server.sh'
