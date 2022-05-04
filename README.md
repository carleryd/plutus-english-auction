# Minting App!
This repo is the basis of my NFT minting app running on https://carleryd.tech
It can be interacted with by connecting to a users [Nami wallet](https://github.com/Berry-Pool/nami-wallet).
The functionality is simple. The user enters a preferred name for the NFT and then sends 5 ada to the wallet running on the backend. Once the backend wallet has confirmed the payment transaction, an NFT with the chosen name is minted and sent to the users Nami wallet.

The app constitutes a REST backend written in Haskell and a frontend in Purescript with the UI framework Halogen. However the main point of this repo is to interact with Plutus smart contracts, so the rest of this readme is all focused on that.

## Plutus smart contract code
The only contract (i.e. validator script) that we interact with is the [minting of the NFT](https://github.com/carleryd/plutus-english-auction/tree/main/src/backend/src/Contract/Token) which is very simple. It consumes a UTXO of the producing wallet and creates an NFT which is sent to a target wallet address.
For us to request payment from the Nami wallet, we produce an unbalanced transaction with the instructions to send an amount of ada to the address of our backend wallet. The frontend logic then fetches this unbalanced transaction using the endpoints of the PAB (explained later) and passes it to the Nami wallet which is then requested to sign and submit this transaction.
Below I explain which tools are used to do what, and why I couldn't make the NFT creation an atomic action instead of splitting it up into two parts.

## Architecture
This app is built using the Plutus Application Backend, or [PAB](https://iohk.io/en/blog/posts/2021/10/28/plutus-application-backend-pab-supporting-dapp-development-on-cardano/) for short, which is a tool built by [IOG](https://iog.io/) as the intended way for building and interacting with DApps. It basically allows you to build, test and deploy smart contracts all in Haskell and Plutus code without having to resort to manually deploy and interact with your contracts using the cardano-cli or other low level tooling. What my app is doing is basically the same as what Lars Brünjes covers in his [PPP lecture](https://www.youtube.com/playlist?list=PLNEK_Ejlx3x2sBWXHdFBRgkzPF6N-1LVi) on using the PAB with the testnet, except I have a lot of UI and wallet interaction to go with it.

On top of the PAB, a few other services need to be running. Firstly [cardano-node](https://github.com/input-output-hk/cardano-node) needs to be running so that the contracts we're building can be submitted to the blockchain. This is the only prerequisite for us to request the payment to be made by the Nami wallet to our backend wallet, as is covered in this [tutorial](https://github.com/input-output-hk/plutus-apps/tree/main/plutus-pab-executables/demo/pab-nami). However, for our backend to produce the NFT we will need to use a couple more tools.

The backend runs an instance of [cardano-wallet](https://github.com/input-output-hk/cardano-wallet) which we use to build the NFT producing transaction. This is what I've referred to as the backend wallet above, and the only reason it's need is because the Nami wallet is not able to do the balancing of this transaction itself, as with the payment transaction. It's a big issue for devs at the moment and has been covered really well by Jay in a [blog post](https://jeyeins.substack.com/p/we-need-better-wallets-on-cardano?r=1avi52&utm_campaign=post&utm_medium=web&s=r) and related [issue](https://github.com/input-output-hk/plutus-apps/issues/249) on the plutus-apps GitHub repo. The creator of the Nami Wallet himself has also commented on the limitation in this [issue](https://github.com/Berry-Pool/nami-wallet/issues/183).
As far as I can gather, other projects running on mainnet today either work with low level tooling or forks of either the PAB or the cardano-wallet to make this work.

The last piece of the puzzle is the [plutus-chain-index](https://github.com/input-output-hk/plutus-apps/tree/main/plutus-chain-index) which communicates with the cardano-node and builds up an index with data needed by the PAB.

With all of these services running, we can launch our Haskell server which can interact with the [endpoints of the PAB](http://carleryd.tech:9080/swagger/swagger-ui/#/), which is how execution of our minting contract works.

# Original and now future intent
The original intent was to build an app which interacted with the [English Auction contract](https://github.com/carleryd/plutus-english-auction/blob/main/src/contracts/src/EnglishAuction.hs) which I've spent most of the time tweaking and testing, but because of above limitations with balancing transactions which consume contracts, I decided to limit the scope for now.
The English Auction contract was originally copied from a past iteration of the Plutus Pioneer Program and can be viewed [here](https://plutus-pioneer-program.readthedocs.io/en/latest/week1.html).
There are also [trace and model tests](https://github.com/carleryd/plutus-english-auction/tree/main/src/contracts/test) set up for that contract.

Depending on priorities I may change this app to interact with the auction contract, but for now I'll leave it as it is, a magnificent NFT minter.
