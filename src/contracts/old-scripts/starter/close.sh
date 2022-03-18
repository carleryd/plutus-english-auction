./cardano-cli transaction build \
	--alonzo-era \
	--testnet-magic 1097911063 \
	--change-address $(cat ./wallets/w1/payment.addr) \
	--tx-in 5120862a70d0760df1610e3c6efdd7e35d20afcc6bd13ee96b50c08ba011129b#1 \
	--tx-in-script-file ./scripts/validators/starter.plutus \
	--tx-in-datum-file ./scripts/data/unit.json \
	--tx-in-redeemer-file ./scripts/data/close-redeemer.json \
	--tx-in-collateral 5120862a70d0760df1610e3c6efdd7e35d20afcc6bd13ee96b50c08ba011129b#0 \
	--protocol-params-file ./scripts/data/protocol.json \
	--out-file ./scripts/output/tx.body

./cardano-cli transaction sign \
	--tx-body-file ./scripts/output/tx.body \
	--signing-key-file ./wallets/w1/payment.skey \
	--testnet-magic 1097911063 \
	--out-file ./scripts/output/tx.signed

./cardano-cli transaction submit \
	--testnet-magic 1097911063 \
	--tx-file ./scripts/output/tx.signed


