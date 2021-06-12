#!/usr/bin/env bash


killall pact-api-server || true

pact-api-server &
