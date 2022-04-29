{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Endpoints
  ( getCurrentSlot,
    checkContractStatus,
    fetchMostRecentTx,
    homeEndpoint,
    baseEndpoints,
    balancesEndpoint,
    postPendingTx,
    checkSuffix,
    getLastTx,
    Transaction (..),
    Output (..),
    Asset (..),
  )
where

import Contract.Utils (unsafeReadAddress, unsafeReadWalletId)
import Control.Monad.IO.Class
import Data.Aeson hiding (json)
import Data.Proxy (Proxy)
import Data.Text (Text, pack)
import Data.Time.Clock (addUTCTime, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX
import Data.Time.Format.ISO8601 (iso8601Show)
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

checkSuffix :: String -> String -> Bool
checkSuffix _ [] = False
checkSuffix suffix (x : xs) = suffix == (x : xs) || checkSuffix suffix xs

-- | Introduced this because "wasm" content type had to be set for Nami wallet to work.
setHeaderContentType :: String -> ActionM ()
setHeaderContentType xs
  | checkSuffix ".js" xs = setHeader "Content-Type" "application/javascript"
  | checkSuffix ".wasm" xs = setHeader "Content-Type" "application/wasm"
  | checkSuffix ".css" xs = setHeader "Content-Type" "text/css"
  | otherwise = setHeader "Content-Type" "text/html"

baseEndpoints :: ScottyM ()
baseEndpoints = do
  get "/" homeEndpoint
  get "/assets/index.js" $ file "./src/assets/index.js"
  get "/assets/images/:image" $ do
    v <- param "image"
    setHeader "Content-Type" "image/gif"
    file ("./src/assets/images/" <> v)
  get "/assets/frontend-dist/:file" $ do
    v <- param "file"
    setHeaderContentType v
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

postPendingTx :: (String -> String -> Address -> IO (Either String String)) -> ScottyM ()
postPendingTx createNFT = do
  post "/pending-tx" $ do
    res <- jsonData :: ActionM PendingTxResponse
    -- TODO: Implement proper parsing of JSON payload
    let txh = filter (/= '\"') (txHash res)
        tn = filter (/= '\"') (tokenName res)
        address = unsafeReadAddress $ filter (/= '\"') (senderAddress res)
    txHashE <- liftIO (createNFT txh tn address)
    case txHashE of
      Left e -> json ("/pending-tx error: " <> show e)
      Right a -> json a

data FindTx = FindTx
  { ftWalletId :: String,
    ftTokenName :: String
  }
  deriving (Generic, Show)

instance ToJSON FindTx

instance FromJSON FindTx

getLastTx :: (WalletId -> String -> IO (Either String String)) -> ScottyM ()
getLastTx getTxHash = do
  get "/find-tx" $ do
    wid <- param "walletId"
    tn <- param "tokenName"
    txHashE <- liftIO (getTxHash (unsafeReadWalletId wid) tn)
    case txHashE of
      Left e -> json ("/last-tx error: " <> show e)
      Right a -> json a

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

data Asset = Asset
  { asset_name :: String,
    quantity :: Integer,
    policy_id :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON Asset

instance ToJSON Asset

newtype Output = Output
  { assets :: [Asset]
  }
  deriving (Show, Eq, Generic)

instance FromJSON Output

instance ToJSON Output

data Transaction = Transaction
  { id :: String,
    outputs :: [Output]
  }
  deriving (Show, Eq, Generic)

instance FromJSON Transaction

instance ToJSON Transaction

-- | Fetch a few recent txs, should get at least one, the one we just sent when minting.
-- | To get most recent one: sort descending and get the head
fetchMostRecentTx :: WalletId -> IO (Either String [Transaction])
fetchMostRecentTx wid = do
  -- We want to filter requests based on how recent they are so that we don't have to fetch
  -- all of the txs that the wallet has ever sent.
  fiveMinAgo <-
    iso8601Show . addUTCTime (secondsToNominalDiffTime (-300))
      <$> getCurrentTime

  let p =
        "start" =: pack fiveMinAgo
          <> "order" =: ("descending" :: Text)
  v <-
    runReq defaultHttpConfig $
      req
        GET
        (http "127.0.0.1" /: "v2" /: "wallets" /: pack (show wid) /: "transactions")
        NoReqBody
        (jsonResponse :: Proxy (JsonResponse [Transaction]))
        (port 8090 <> p)
  let c = responseStatusCode v
  if c == 200
    then do
      let newTxs = responseBody v
          txE =
            if null newTxs
              then Left "Received no tx"
              else Right newTxs
      return txE
    else return $ Left ("ERROR: " ++ show c)
