{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Endpoints
  ( startServer,
  )
where

-- import Blockfrost.Types.Cardano.Addresses (AddressUTXO)

import Blockfrost.Types.Shared
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson hiding (json)
import Data.Text (pack)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as LazyText
import GHC.Generics (Generic)
import Generics.Generic.Aeson
import qualified Lib
import Network.Wai.Middleware.Cors
import TxListener (txListener)
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

data PendingTxResponse = PendingTxResponse
  { txHash :: String,
    tokenName :: String
  }
  deriving (Generic, Show)

instance ToJSON PendingTxResponse

stripSettings :: Settings
stripSettings = defaultSettings

instance FromJSON PendingTxResponse

postPendingTx :: ScottyM ()
postPendingTx = do
  post "/pending-tx" $ do
    res <- jsonData :: ActionM PendingTxResponse
    -- TODO: Implement proper parsing of JSON payload
    let txh = (TxHash . pack . filter (/= '\"')) (txHash res)
        tn = filter (/= '\"') (tokenName res)
    liftIO $ txListener txh tn
    text $ LazyText.pack ("Success, " <> show txh)

startServer :: IO ()
startServer = do
  wb <- Lib.getBlockfrostUtxos

  let endpoints =
        baseEndpoints
          <> balancesEndpoint wb
          <> postPendingTx
  -- <> pabEndpoints

  scotty 3000 (middleware simpleCors <> endpoints)
