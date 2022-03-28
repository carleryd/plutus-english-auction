ADA_UTXO="44225f4cda11f6f9faf4dd53e842a4d153be5a5ca8cd7e48237d61c2576451e8#0"
TUI_UTXO="44225f4cda11f6f9faf4dd53e842a4d153be5a5ca8cd7e48237d61c2576451e8#1"
MIN_ADA="2000000 lovelace"
TUI_TOKEN="545549"
MOOSE_TOKEN="4d4f4f5345"
POLICY_ID=$(cat ./scripts/auction/tokens/policy/policyID)

TUI_SEND=1
MOOSE_SEND=0

CURRENT_TUI_AMOUNT=1
CURRENT_MOOSE_AMOUNT=0

TUI_RECEIVE=$((CURRENT_TUI_AMOUNT - TUI_SEND))
MOOSE_RECEIVE=$((CURRENT_MOOSE_AMOUNT - MOOSE_SEND))

TOKEN_TO_SENDER="$TUI_RECEIVE $POLICY_ID.$TUI_TOKEN + $MOOSE_RECEIVE $POLICY_ID.$MOOSE_TOKEN"
TOKEN_TO_SCRIPT="$TUI_SEND $POLICY_ID.$TUI_TOKEN + $MOOSE_SEND $POLICY_ID.$MOOSE_TOKEN"

cardano-cli transaction build \
	--alonzo-era \
	--testnet-magic 1097911063 \
	--change-address "$(cat ./scripts/wallets/w1/payment.addr)" \
	--tx-in $ADA_UTXO \
	--tx-in $TUI_UTXO \
	--tx-out "$(cat ./scripts/wallets/w1/payment.addr) $MIN_ADA + $TOKEN_TO_SENDER" \
	--tx-out "$(cat ./scripts/auction/validators/auction.addr) $MIN_ADA + $TOKEN_TO_SCRIPT" \
	--tx-out-datum-hash-file ./scripts/auction/data/start-datum.json \
	--out-file ./scripts/output/tx.body

cardano-cli transaction sign \
	--tx-body-file ./scripts/output/tx.body \
	--signing-key-file ./scripts/wallets/w1/payment.skey \
	--testnet-magic 1097911063 \
	--out-file ./scripts/output/tx.signed

cardano-cli transaction submit \
	--testnet-magic 1097911063 \
	--tx-file ./scripts/output/tx.signed
