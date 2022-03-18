./cardano-cli transaction build \
	--alonzo-era \
	--testnet-magic 1097911063 \
	--change-address $(cat ./wallets/w1/payment.addr) \
	--tx-in 8b4ff0e39f61cfeac49767b5466ded13582112b9ba4728c9c344cc72597d1419#0 \
	--tx-out "$(cat ./scripts/validators/addresses/starter.addr) 5000000 lovelace" \
	--tx-out-datum-hash-file ./scripts/data/unit.json \
	--out-file ./scripts/output/tx-2.body

./cardano-cli transaction sign \
	--tx-body-file ./scripts/output/tx-2.body \
	--signing-key-file ./wallets/w1/payment.skey \
	--testnet-magic 1097911063 \
	--out-file ./scripts/output/tx-2.signed

./cardano-cli transaction submit \
	--testnet-magic 1097911063 \
	--tx-file ./scripts/output/tx-2.signed
