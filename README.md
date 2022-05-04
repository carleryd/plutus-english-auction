# 🪙 Minting App 💸
This repo is the basis of my NFT minting app running on https://carleryd.tech

It can be interacted with by connecting to a users [Nami wallet](https://github.com/Berry-Pool/nami-wallet).
The functionality is simple. The user enters a preferred name for the NFT and then sends 5 ada to the wallet running on the backend. Once the backend wallet has confirmed the payment transaction, an NFT with the chosen name is minted and sent to the users Nami wallet.

The app constitutes a REST backend written in Haskell and a frontend in Purescript with the UI framework [Halogen](https://github.com/purescript-halogen/purescript-halogen). However the main point of this repo is to interact with Plutus smart contracts, so the rest of this readme is all focused on that.

## Plutus smart contract code
The only contract (i.e. validator script) that is interacted with is the [minting of the NFT](https://github.com/carleryd/plutus-english-auction/tree/main/src/backend/src/Contract/Token) which is very simple. It consumes a UTXO of the wallet producing the transaction and creates an NFT which is sent to a target wallet address.

For the app to request payment from the Nami wallet, an unbalanced transaction is produced with the instructions to send an amount of ada to the address of our backend wallet. The frontend logic then fetches this unbalanced transaction using the endpoints of the PAB (explained later) and passes it to the Nami wallet which is then requested to sign and submit this transaction.

Below I explain which tools are used to do what, and why I couldn't make the NFT creation an atomic action and instead chose to split it up into two parts.

## Cardano tooling
This app is built using the Plutus Application Backend, or [PAB](https://iohk.io/en/blog/posts/2021/10/28/plutus-application-backend-pab-supporting-dapp-development-on-cardano/) for short, which is a tool built by [IOG](https://iog.io/) as the intended way for building and interacting with DApps. It basically allows you to build, test and deploy smart contracts all in Haskell and Plutus code without having to resort to manually deploy and interact with your contracts using the cardano-cli or other low level tooling. What my app is doing is basically the same as what Lars Brünjes covers in his [PPP lecture](https://www.youtube.com/playlist?list=PLNEK_Ejlx3x2sBWXHdFBRgkzPF6N-1LVi) on using the PAB with the testnet, except I have a lot of UI and wallet interaction to go with it.

### Browser wallet limitations
Had it not been for the limitations in supporting DApps in web browsers (see [CIP-30](https://cips.cardano.org/cips/cip30/)) this app would have been significantly simpler because it wouldn't have to submit transactions itself. This is the reason for why the app first requests a payment from the Nami wallet, and then produces the NFT transaction using a [cardano-wallet](https://github.com/input-output-hk/cardano-wallet) instance running on the backend.

The problem boils down to the complexity of calculating the fees for executing smart contract code (i.e. Plutus on-chain validators). This has been extensively covered by a user named Jay in a great [blog post](https://jeyeins.substack.com/p/we-need-better-wallets-on-cardano?r=1avi52&utm_campaign=post&utm_medium=web&s=r) and related [issue](https://github.com/input-output-hk/plutus-apps/issues/249).

The difficulty of balancing transactions in the browser has also been explained by the author of the Nami Wallet in this [issue](https://github.com/Berry-Pool/nami-wallet/issues/183).

As far as I can gather, other projects running on mainnet today either work with low level tooling or forks of either the PAB or the cardano-wallet to make this work.

Because of this, I'm going to introduce the different services needed for submitting transactions that interact with smart contracts below.

### Cardano node, wallet and chain-index
The bare requirements for using the PAB is to connect it to the socket of a running [cardano-node](https://github.com/input-output-hk/cardano-node) instance. This is required for us to send the minting transaction onto the blockchain.

Secondly we need to run an instance of a [cardano-wallet](https://github.com/input-output-hk/cardano-wallet) which pays for the fees and produces the UTXO needed for minting the NFT.

The last piece of the puzzle is the [plutus-chain-index](https://github.com/input-output-hk/plutus-apps/tree/main/plutus-chain-index) which communicates with the cardano-node and builds up an index with data needed by the PAB.

With all these pieces in place, our REST backend can now accept requests from the frontend to call endpoints on the running PAB instance which instructs it to run the miniting process 🎉

For testing purposes I've exposed different endpoints such as the [PAB](http://carleryd.tech:9080/swagger/swagger-ui/#/) and [plutus-chain-index](http://carleryd.tech:9083/swagger/swagger-ui/). If you're clever you could use them to bypass the 5 ada fee, and drain my wallet in the meantime!

# Future work
The original intent was to build an app which interacted with the [English Auction contract](https://github.com/carleryd/plutus-english-auction/blob/main/src/contracts/src/EnglishAuction.hs). This is the contract code I've spent most of my time with, but because of above limitations with browser wallets, I decided to limit the scope for now.

The English Auction contract was originally copied from a past iteration of the Plutus Pioneer Program and can be viewed [here](https://plutus-pioneer-program.readthedocs.io/en/latest/week1.html).

There are also [trace and model tests](https://github.com/carleryd/plutus-english-auction/tree/main/src/contracts/test) set up for that contract.

Perhaps I'll upgrade this app one day to interact with the auction contract, but for now I'll leave it as it is, a magnificent NFT minter!
