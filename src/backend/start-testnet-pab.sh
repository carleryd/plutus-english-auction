#!/bin/bash

cabal run -- pab-server \
  --config testnet/pab-config.yml webserver \
  --passphrase kakmonster123
