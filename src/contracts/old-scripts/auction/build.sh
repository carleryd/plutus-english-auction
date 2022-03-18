echo "Current auction.plutus address"
cat ./scripts/auction/validators/auction.addr

address=$(./cardano-cli address build \
    --payment-script-file ./scripts/auction/validators/auction.plutus \
    --testnet-magic 1097911063 \
)

echo "$address" > scripts/auction/validators/auction.addr

echo "\nNew auction.plutus address"
cat ./scripts/auction/validators/auction.addr

echo "\n\nCurrent start-datum.json"
cat ./scripts/auction/data/start-datum-hash.addr

startDatumHash=$(./cardano-cli transaction hash-script-data \
    --script-data-file ./scripts/auction/data/start-datum.json \
)

echo "$startDatumHash" > scripts/auction/data/start-datum-hash.addr

echo "\nNew start-datum.json"
cat ./scripts/auction/data/start-datum-hash.addr

echo "\n\nCurrent bid-w2-datum-hash.addr"
cat ./scripts/auction/data/bid-w2-datum-hash.addr

bidDatumHash=$(./cardano-cli transaction hash-script-data \
    --script-data-file ./scripts/auction/data/bid-w2-datum.json \
)

echo "$bidDatumHash" > scripts/auction/data/bid-w2-datum-hash.addr

echo "\nNew bid-w2-datum-hash.addr"
cat ./scripts/auction/data/bid-w2-datum-hash.addr
