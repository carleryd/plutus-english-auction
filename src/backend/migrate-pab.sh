#!/bin/bash

cabal run -- pab-server \
  --config testnet/pab-config.yml migrate
