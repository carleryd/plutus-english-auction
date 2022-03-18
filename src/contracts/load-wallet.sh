#!/bin/bash

walletSrc=$1

curl -H "content-type: application/json" -XPOST \
    -d @$walletSrc \
    localhost:8090/v2/wallets
