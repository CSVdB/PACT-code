#!/usr/bin/env bash


killall pact-web-server || true

~/.local/bin/pact-web-server &
