# echo -n <TOKEN_NAME> | xxd -b -ps -c 80 | tr -d '\n'
TUI_TOKEN="545549"
MOOSE_TOKEN="4d4f4f5345"
TOKEN_AMOUNT="100"
ADDRESS="$(cat ./scripts/wallets/w1/payment.addr)"
WALLET_UTXO="03c9e85de45da4f6f73438d3e29a4e748e1021a22e4a3b8d7881b13b31332e5c#0"
POLICY_ID=$(cat ./scripts/auction/tokens/policy/policyID)

TUI="$POLICY_ID.$TUI_TOKEN"
MOOSE="$POLICY_ID.$MOOSE_TOKEN"

MILLION=1000000
funds="$((200 * MILLION))"
fee=$(cardano-cli transaction calculate-min-fee \
	--tx-body-file ./scripts/auction/tokens/matx.raw \
	--tx-in-count 1 \
	--tx-out-count 1 \
	--witness-count 2 \
	--testnet-magic 1097911063 \
	--protocol-params-file ./scripts/data/protocol.json | cut -d " " -f1 \
)
OUTPUT=$((funds - fee))

echo "### Building matx.raw ###"
cardano-cli transaction build-raw \
	--fee "$fee" \
	--tx-in $WALLET_UTXO \
	--tx-out "$ADDRESS + $OUTPUT + $TOKEN_AMOUNT $TUI + $TOKEN_AMOUNT $MOOSE" \
	--mint="$TOKEN_AMOUNT $TUI + $TOKEN_AMOUNT $MOOSE" \
	--minting-script-file ./scripts/auction/tokens/policy/policy.script \
	--out-file ./scripts/auction/tokens/matx.raw

echo "\n\n### Signing matx.raw with wallet 1 and policy ###"
cardano-cli transaction sign  \
	--signing-key-file ./scripts/wallets/w1/payment.skey  \
	--signing-key-file ./scripts/auction/tokens/policy/policy.skey  \
	--testnet-magic 1097911063 \
	--tx-body-file ./scripts/auction/tokens/matx.raw \
	--out-file ./scripts/auction/tokens/matx.signed

echo "\n\n### Submitting matx.signed ###"
cardano-cli transaction submit \
	--tx-file ./scripts/auction/tokens/matx.signed \
	--testnet-magic 1097911063
