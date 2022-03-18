IN_UTXO="4960b2d164ee4d3e1b5cda1e4d87dc413e3ea617f9c303c8055c06ca617b3860#2"
MIN_ADA="2000000 lovelace"
TUI_TOKEN=$(echo -n "TUI" | xxd -b -ps -c 80 | tr -d '\n')
MOOSE_TOKEN=$(echo -n "MOOSE" | xxd -b -ps -c 80 | tr -d '\n')
POLICY_ID=$(cat ./scripts/auction/tokens/policy/policyID)

TUI_SEND=10
MOOSE_SEND=10

TUI="$POLICY_ID.$TUI_TOKEN"
MOOSE="$POLICY_ID.$MOOSE_TOKEN"


./cardano-cli transaction build \
	--alonzo-era \
	--testnet-magic 1097911063 \
	--change-address $(cat ./wallets/w2/payment.addr) \
	--tx-in $IN_UTXO \
	--tx-out "$(cat ./wallets/w1/payment.addr) $MIN_ADA + $TUI_SEND $TUI + $MOOSE_SEND $MOOSE" \
	--out-file ./scripts/output/tx.body

./cardano-cli transaction sign \
	--tx-body-file ./scripts/output/tx.body \
	--signing-key-file ./wallets/w2/payment.skey \
	--testnet-magic 1097911063 \
	--out-file ./scripts/output/tx.signed

./cardano-cli transaction submit \
	--testnet-magic 1097911063 \
	--tx-file ./scripts/output/tx.signed
