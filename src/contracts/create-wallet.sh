#!/bin/bash
# USAGE: Pass 3 arguments to create a wallet which can then be used with `load-wallet.sh`

name=$1
passphrase=$2
file=$3
echo "creating wallet with name $name passphrase $passphrase"

phrase=$(cardano-wallet recovery-phrase generate)

x=''
sep=''
for word in $phrase
do
    x=$x$sep'"'$word'"'
    sep=', '
done

cat > $file <<- EOM
{ "name": "$name"
, "mnemonic_sentence": [$x]
, "passphrase": "$passphrase"
}
EOM
echo "saved restoration file to $file"
