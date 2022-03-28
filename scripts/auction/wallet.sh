# Create payment verification and payment signing keys
cardano-cli address key-gen \
    --verification-key-file payment.vkey \
    --signing-key-file payment.skey

# Generate payment.addr (no staking)
cardano-cli address build \
    --payment-verification-key-file payment.vkey \
    --out-file payment.addr \
    --testnet-magic 1097911063

# Generate pubkeyhash from payment.addr
cardano-cli address key-hash \
    --payment-verification-key "$(cat ./scripts/wallets/w1/payment.vkey)"
