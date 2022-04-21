#!/bin/bash

amt=$1
tn=$2
printf "minting $1 coins of token $2 using \nAddress: $ADDRESS\nWallet id: $WALLETID\n\n"

cabal run mint-token -- $1 $2 $WALLETID $ADDRESS
