# Introduction
This stack has the ambition to be a frontend written in PureScript and a backend in Haskell which is integrated with the [Plutus PAB](https://github.com/input-output-hk/plutus-apps).

Currently most of the work has gone into the [Plutus Contracts](https://github.com/carleryd/plutus-english-auction/tree/main/src/contracts), and in particular the [Trace and Model testing](https://github.com/carleryd/plutus-english-auction/tree/main/src/contracts/test) of the [English Auction contract](https://github.com/carleryd/plutus-english-auction/blob/main/src/contracts/src/EnglishAuction.hs).
It was originally copied from lecture 1 of the [Plutus Pioneer Program](https://www.youtube.com/channel/UCcAwSpbpQDDzEDRQqcDH8Iw/playlists) but has diverged after undergoing several modifications to work with newer versions of [plutus-apps](https://github.com/input-output-hk/plutus-apps).

The Haskell and PureScript stack is fully functional, which at the moment serves PureScript code which interacts with [Blockfrost](https://blockfrost.io/) endpoints.

# Future work
I've done a lot of work trying to get the PAB to work with the testnet. I believe my current problem is related to the [plutus-chain-index](https://github.com/input-output-hk/plutus-apps/tree/main/plutus-chain-index) not interacting properly with the [cardano-wallet](https://github.com/input-output-hk/cardano-wallet).
See open [issues].

Once I get the PAB to work properly, I plan to modify the PureScript frontend to connect wallets and interact with the EnglishAuction.hs contract.
