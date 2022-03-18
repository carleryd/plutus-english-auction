./cardano-cli transaction build \
	--alonzo-era \
	--testnet-magic 1097911063 \
	--change-address $(cat ./wallets/w2/payment.addr) \
	--tx-in 8b4ff0e39f61cfeac49767b5466ded13582112b9ba4728c9c344cc72597d1419#1 \
	--tx-in-script-file ./scripts/validators/starter.plutus \
	--tx-in-datum-file ./scripts/data/publish-params.json \
	--tx-in-redeemer-file ./scripts/data/grab-redeemer.json \
	--tx-in-collateral c4a5b55d2e0a8a44e7e78a9d37249396ae787664b5e859a54b413e1e7686695b#1 \
	--protocol-params-file ./scripts/data/protocol.json \
	--out-file ./scripts/output/tx.body

./cardano-cli transaction sign \
	--tx-body-file ./scripts/output/tx.body \
	--signing-key-file ./wallets/w2/payment.skey \
	--testnet-magic 1097911063 \
	--out-file ./scripts/output/tx.signed

./cardano-cli transaction submit \
	--testnet-magic 1097911063 \
	--tx-file ./scripts/output/tx.signed

