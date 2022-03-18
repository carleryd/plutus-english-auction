# Has plenty Ada
WALLET_UTXO="fffee00950333af946229f9d04b90d70baa2fbf4432fbff0b6d416b0c479c5ae#0"
SCRIPT_UTXO="b4b4ec7b4a3da0ef45488e77f24c9c298981e9e15613144be312c7c113ff942d#1"

TUI_TOKEN="545549"
MOOSE_TOKEN="4d4f4f5345"
POLICY_ID=$(cat ./scripts/auction/tokens/policy/policyID)

TUI_RECEIVE=1
MOOSE_RECEIVE=0
SCRIPT_FUNDS=2000000

TOKEN_TO_WALLET="$TUI_RECEIVE $POLICY_ID.$TUI_TOKEN + $MOOSE_RECEIVE $POLICY_ID.$MOOSE_TOKEN"

START_DATUM_FILE="./scripts/auction/data/start-datum.json"
BID_W2_DATUM_FILE="./scripts/auction/data/bid-w2-datum.json"
DATUM_FILE=$BID_W2_DATUM_FILE

FEE=548954
ADA_TO_WALLET="$((SCRIPT_FUNDS - FEE)) lovelace"

./cardano-cli transaction build \
	--alonzo-era \
	--testnet-magic 1097911063 \
	--change-address "$(cat ./wallets/w1/payment.addr)" \
    --tx-in $WALLET_UTXO \
    --tx-in $SCRIPT_UTXO \
    --tx-in-script-file ./scripts/auction/validators/auction.plutus \
    --tx-in-datum-file $DATUM_FILE \
    --tx-in-redeemer-file ./scripts/auction/data/abort-redeemer.json \
    --tx-in-collateral $WALLET_UTXO \
	--tx-out "$(cat ./wallets/w1/payment.addr) $ADA_TO_WALLET + $TOKEN_TO_WALLET" \
    --required-signer-hash "$(cat ./wallets/w1/pubkeyhash.addr)" \
    --protocol-params-file ./scripts/data/protocol.json \
	--out-file ./scripts/output/tx.body

./cardano-cli transaction sign \
	--testnet-magic 1097911063 \
	--tx-body-file ./scripts/output/tx.body \
	--signing-key-file ./wallets/w1/payment.skey \
	--out-file ./scripts/output/tx.signed

./cardano-cli transaction submit \
	--testnet-magic 1097911063 \
	--tx-file ./scripts/output/tx.signed
