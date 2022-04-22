# Setup
Temporary measure to give us exposure to non-public api

$ git clone --branch 2.3.6 https://github.com/Berry-Pool/nami-wallet.git lib/nami-wallet

Then install `purescript` and `spago` and run

$ yarn && yarn build:spago && yarn build:webpack

Output will be generated in backend's assets, so now you can run the server!
