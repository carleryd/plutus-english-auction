COLLATERAL_UTXO="b4b4ec7b4a3da0ef45488e77f24c9c298981e9e15613144be312c7c113ff942d#0"
SCRIPT_UTXO="7eb218dcd4de1689b13dd2ada5201d35af053b7ea9835cbcc5b57fcd97518b95#2"

MILLION=1000000
BID=$((10 * MILLION))
MIN_ADA=$((2 * MILLION))

TUI_TOKEN="545549"
POLICY_ID=$(cat ./scripts/auction/tokens/policy/policyID)

TUI_SEND=1

TOKEN_TO_SCRIPT="$TUI_SEND $POLICY_ID.$TUI_TOKEN"

ADA_TO_SCRIPT="$((MIN_ADA + BID)) lovelace"

cardano-cli transaction build \
	--alonzo-era \
	--testnet-magic 1097911063 \
	--change-address "$(cat ./wallets/w2/payment.addr)" \
    --tx-in $COLLATERAL_UTXO \
    --tx-in $SCRIPT_UTXO \
    --tx-in-script-file ./scripts/auction/validators/auction.plutus \
    --tx-in-datum-file ./scripts/auction/data/start-datum.json \
    --tx-in-redeemer-file ./scripts/auction/data/bid-w2-redeemer.json \
    --tx-in-collateral $COLLATERAL_UTXO \
	--tx-out "$(cat ./scripts/auction/validators/auction.addr) $ADA_TO_SCRIPT + $TOKEN_TO_SCRIPT" \
    --tx-out-datum-embed-file ./scripts/auction/data/bid-w2-datum.json \
    --required-signer-hash "$(cat ./wallets/w2/pubkeyhash.addr)" \
    --protocol-params-file ./scripts/data/protocol.json \
	--out-file ./scripts/output/tx.body

cardano-cli transaction sign \
	--testnet-magic 1097911063 \
	--tx-body-file ./scripts/output/tx.body \
	--signing-key-file ./wallets/w2/payment.skey \
	--out-file ./scripts/output/tx.signed

cardano-cli transaction submit \
	--testnet-magic 1097911063 \
	--tx-file ./scripts/output/tx.signed
