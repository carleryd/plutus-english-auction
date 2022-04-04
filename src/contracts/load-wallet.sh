#!/bin/bash
# USAGE: This takes the source of a wallet in the format created by `create-wallet.sh`

walletSrc=$1

curl -H "content-type: application/json" -XPOST \
    -d @$walletSrc \
    192.168.1.103:8090/v2/wallets
