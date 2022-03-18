echo "### Wallet 1 UTXOs ###"
./cardano-cli query utxo --address $(cat ./wallets/w1/payment.addr) --testnet-magic 1097911063

echo "\n\n### Wallet 2 UTXOs ###"
./cardano-cli query utxo --address $(cat ./wallets/w2/payment.addr) --testnet-magic 1097911063

echo "\n\n### Auction script UTXOs ###"
./cardano-cli query utxo --address $(cat ./scripts/auction/validators/auction.addr) --testnet-magic 1097911063
