{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Endpoints
  ( getCurrentSlot,
    checkContractStatus,
    fetchTxs,
    homeEndpoint,
    baseEndpoints,
    balancesEndpoint,
    postPendingTx,
  )
where

import Blockfrost.Types.Shared hiding (Address, Slot)
import Contract.Utils (unsafeReadAddress, unsafeReadWalletId)
import Control.Monad.IO.Class
import Data.Aeson hiding (json)
import Data.Proxy (Proxy)
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Ledger (Slot (..))
import Ledger.Address (Address)
import Network.HTTP.Req
import Plutus.Contract (ContractInstanceId)
import Plutus.PAB.Events.Contract (ContractInstanceId (unContractInstanceId))
import qualified Utils
import Wallet.Emulator.Wallet (WalletId (..))
import Web.Scotty
import Prelude hiding (id)

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

postPendingTx :: (TxHash -> String -> Address -> IO (Either String TxHash)) -> ScottyM ()
postPendingTx createNFT = do
  post "/pending-tx" $ do
    res <- jsonData :: ActionM PendingTxResponse
    -- TODO: Implement proper parsing of JSON payload
    let txh = (TxHash . pack . filter (/= '\"')) (txHash res)
        tn = filter (/= '\"') (tokenName res)
        address = unsafeReadAddress $ filter (/= '\"') (senderAddress res)
    txHashE <- liftIO (createNFT txh tn address)
    case txHashE of
      Left e -> json ("/pending-tx error: " <> show e)
      Right a -> json ("/pending-tx success: " <> show a)

newtype Tip = Tip
  { tipSlot :: Slot
  }
  deriving (Show, Eq, Generic)

instance FromJSON Tip

instance ToJSON Tip

-- TODO: Maybe remove
getCurrentSlot :: IO (Either String Slot)
getCurrentSlot = do
  v <-
    runReq defaultHttpConfig $
      req
        GET
        (http "127.0.0.1" /: "tip")
        NoReqBody
        (jsonResponse :: Proxy (JsonResponse Tip))
        (port 9083)
  let c = responseStatusCode v
  if c == 200
    then do
      return $ Right $ (tipSlot . responseBody) v
    else return $ Left ("ERROR: " ++ show c)

newtype CicWallet = CicWallet
  { getWalletId :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON CicWallet

instance ToJSON CicWallet

newtype ContractStatus = ContractStatus
  { cicWallet :: CicWallet
  }
  deriving (Show, Eq, Generic)

instance FromJSON ContractStatus

instance ToJSON ContractStatus

checkContractStatus :: ContractInstanceId -> IO (Either String WalletId)
checkContractStatus cid = do
  v <-
    runReq defaultHttpConfig $
      req
        GET
        ( http "127.0.0.1"
            /: "api"
            /: "contract"
            /: "instance"
            /: (pack . show . unContractInstanceId) cid
            /: "status"
        )
        NoReqBody
        (jsonResponse :: Proxy (JsonResponse ContractStatus))
        (port 9080)
  let c = responseStatusCode v
  if c == 200
    then do
      let walletId = (getWalletId . cicWallet) (responseBody v)
      return $ Right (unsafeReadWalletId walletId)
    else return $ Left ("ERROR: " ++ show c)

newtype InsertedAt = InsertedAt
  {absolute_slot_number :: Integer}
  deriving (Show, Eq, Generic)

instance FromJSON InsertedAt

instance ToJSON InsertedAt

data Transaction = Transaction
  { id :: String,
    inserted_at :: InsertedAt
  }
  deriving (Show, Eq, Generic)

instance FromJSON Transaction

instance ToJSON Transaction

-- | Fetch a few recent txs, should get at least one, the one we just sent when minting.
-- | To get most recent one: sort descending and get the head
fetchTxs :: String -> WalletId -> IO (Either String TxHash)
fetchTxs fromTimestamp wid = do
  let params =
        "start" =: pack fromTimestamp
          <> "order" =: ("descending" :: Text)
  v <-
    runReq defaultHttpConfig $
      req
        GET
        (http "127.0.0.1" /: "v2" /: "wallets" /: pack (show wid) /: "transactions")
        NoReqBody
        (jsonResponse :: Proxy (JsonResponse [Transaction]))
        (port 8090 <> params)
  let c = responseStatusCode v
  if c == 200
    then do
      let newTxs = responseBody v
          txE =
            if null newTxs
              then Left "Received no tx"
              else Right $ TxHash $ pack ((id . head) newTxs)
      return txE
    else return $ Left ("ERROR: " ++ show c)
