{-# LANGUAGE OverloadedStrings #-}

module Endpoints
  ( startServer,
  )
where

import Blockfrost.Types.Cardano.Addresses (AddressUTXO)
import Data.Aeson (ToJSON (..))
import qualified Lib
import Web.Scotty

homeEndpoint :: ActionM ()
homeEndpoint = do
  setHeader "Content-Type" "text/html"
  file "./src/assets/index.html"

myData :: String
myData = "hello from backend!"

baseEndpoints :: ScottyM ()
baseEndpoints = do
  get "/" homeEndpoint
  get "/assets/index.js" $ file "./src/assets/index.js"

-- balancesEndpoint :: Lib.WalletBalances -> ScottyM ()
-- balancesEndpoint wb = do
--   get "/balances" $ json $ toJSON wb

balancesEndpoint :: Lib.WalletBalances -> ScottyM ()
balancesEndpoint wb = do
  get "/balances" $ json $ toJSON wb

startServer :: IO ()
startServer = do
  wb <- Lib.getUtxos

  scotty 3000 (baseEndpoints <> balancesEndpoint wb)
