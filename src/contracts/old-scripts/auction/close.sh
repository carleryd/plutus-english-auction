# Has plenty Ada
WALLET_UTXO="bef5ecbce403838d7b598ba72eb7d6e6b12bbb55d9a32815d5de5b98fab56e29#0"
SCRIPT_UTXO="bef5ecbce403838d7b598ba72eb7d6e6b12bbb55d9a32815d5de5b98fab56e29#1"

MILLION=1000000
BID=$((10 * MILLION))
MIN_ADA=$((2 * MILLION))

TUI_TOKEN="545549"
POLICY_ID=$(cat ./scripts/auction/tokens/policy/policyID)

BIDDER_ADDR=$(cat ./wallets/w2/payment.addr)
ADA_TO_BIDDER="$((MIN_ADA)) lovelace"
TOKEN_TO_BIDDER="1 $POLICY_ID.$TUI_TOKEN"

echo "ADA_TO_BIDDER: $ADA_TO_BIDDER"
echo "TOKEN_TO_BIDDER: $TOKEN_TO_BIDDER"

SELLER_ADDR=$(cat ./wallets/w1/payment.addr)
ADA_TO_SELLER="$((BID)) lovelace"
TOKEN_TO_SELLER=""

echo "ADA_TO_SELLER: $ADA_TO_SELLER"
echo "TOKEN_TO_SELLER: $TOKEN_TO_SELLER"

START_DATUM_FILE="./scripts/auction/data/start-datum.json"
BID_W2_DATUM_FILE="./scripts/auction/data/bid-w2-datum.json"
DATUM_FILE=$BID_W2_DATUM_FILE

./cardano-cli transaction build \
	--alonzo-era \
	--testnet-magic 1097911063 \
	--change-address "$(cat ./wallets/w2/payment.addr)" \
    --tx-in $WALLET_UTXO \
    --tx-in $SCRIPT_UTXO \
    --tx-in-script-file ./scripts/auction/validators/auction.plutus \
    --tx-in-datum-file $DATUM_FILE \
    --tx-in-redeemer-file ./scripts/auction/data/close-redeemer.json \
    --tx-in-collateral $WALLET_UTXO \
	--tx-out "$BIDDER_ADDR $ADA_TO_BIDDER + $TOKEN_TO_BIDDER" \
	--tx-out "$SELLER_ADDR $ADA_TO_SELLER" \
    --required-signer-hash "$(cat ./wallets/w2/pubkeyhash.addr)" \
    --protocol-params-file ./scripts/data/protocol.json \
	--out-file ./scripts/output/tx.body

./cardano-cli transaction sign \
	--testnet-magic 1097911063 \
	--tx-body-file ./scripts/output/tx.body \
	--signing-key-file ./wallets/w2/payment.skey \
	--out-file ./scripts/output/tx.signed

./cardano-cli transaction submit \
	--testnet-magic 1097911063 \
	--tx-file ./scripts/output/tx.signed
