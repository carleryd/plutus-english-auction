{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Endpoints
  ( startServer,
  )
where

import Blockfrost.Types.Shared
import Contract.Utils (unsafeReadAddress)
import Control.Monad.IO.Class
import Data.Aeson hiding (json)
import Data.Text (pack)
import qualified Data.Text.Lazy as LazyText
import GHC.Generics (Generic)
import Network.Wai.Middleware.Cors
import TxListener (txListener)
import qualified Utils
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

balancesEndpoint :: Utils.WalletBalances -> ScottyM ()
balancesEndpoint wb = do
  get "/balances" $ json $ toJSON wb

data PendingTxResponse = PendingTxResponse
  { txHash :: String,
    tokenName :: String,
    senderAddress :: String
  }
  deriving (Generic, Show)

instance ToJSON PendingTxResponse

instance FromJSON PendingTxResponse

postPendingTx :: ScottyM ()
postPendingTx = do
  post "/pending-tx" $ do
    res <- jsonData :: ActionM PendingTxResponse
    -- TODO: Implement proper parsing of JSON payload
    let txh = (TxHash . pack . filter (/= '\"')) (txHash res)
        tn = filter (/= '\"') (tokenName res)
        address = unsafeReadAddress $ filter (/= '\"') (senderAddress res)
    liftIO $ txListener txh tn address
    json ("hello" :: String)

startServer :: IO ()
startServer = do
  wb <- Utils.getBlockfrostUtxos

  let endpoints =
        baseEndpoints
          <> balancesEndpoint wb
          <> postPendingTx

  scotty 3000 (middleware simpleCors <> endpoints)
