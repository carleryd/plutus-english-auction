{-# LANGUAGE OverloadedStrings #-}

module Endpoints
  ( startServer,
  )
where

-- import Blockfrost.Types.Cardano.Addresses (AddressUTXO)

import Control.Monad
import Data.Aeson (ToJSON (..))
import qualified Lib
import Network.Wai.Middleware.Cors
import Web.Scotty

homeEndpoint :: ActionM ()
homeEndpoint = do
  setHeader "Content-Type" "text/html"
  file "./src/assets/index.html"

baseEndpoints :: ScottyM ()
baseEndpoints = do
  get "/" homeEndpoint
  get "/assets/index.js" $ file "./src/assets/index.js"
  get "/assets/frontend-dist/:file" $ do
    -- TODO: This probably shouldn't be set on all files, but was required for the .wasm files we serve
    setHeader "Content-Type" "application/wasm"
    v <- param "file"
    file ("./src/assets/frontend-dist/" <> v)

-- pabEndpoints :: ScottyM ()
-- pabEndpoints =
--   get "/api/contract/instance/:cid/status" $ do
--     cid <- param "cid"
--     void next
--     redirect ("http://localhost:8010/proxy/api/contract/instance/" <> cid <> "/status")

-- balancesEndpoint :: Lib.WalletBalances -> ScottyM ()
-- balancesEndpoint wb = do
--   get "/balances" $ json $ toJSON wb

balancesEndpoint :: Lib.WalletBalances -> ScottyM ()
balancesEndpoint wb = do
  get "/balances" $ json $ toJSON wb

startServer :: IO ()
startServer = do
  wb <- Lib.getBlockfrostUtxos

  let endpoints =
        baseEndpoints
          <> balancesEndpoint wb
  -- <> pabEndpoints

  scotty 3000 (middleware simpleCors <> endpoints)
