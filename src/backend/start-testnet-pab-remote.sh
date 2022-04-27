#!/bin/bash

cabal run -- pab-server \
  --config testnet/pab-config-remote.yml webserver \
  --passphrase kakmonster123
