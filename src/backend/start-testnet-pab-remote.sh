#!/bin/bash

cabal run -- token-pab \
  --config testnet/pab-config-remote.yml webserver \
  --passphrase kakmonster123
