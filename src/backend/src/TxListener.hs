{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TxListener where

import Blockfrost.Types.Shared hiding (Address, Slot, unSlot)
import Contract.Utils (unsafeReadWalletId)
import Data.Aeson
import Data.Either.Extras (unsafeFromEither)
import Data.Proxy (Proxy)
import Data.Text (Text, pack)
import Data.Time (addUTCTime, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX
import Data.Time.Format.ISO8601 (iso8601Show)
import GHC.Conc (threadDelay)
import GHC.Generics (Generic)
import Ledger (Slot (..))
import Ledger.Address (Address)
import MintToken (mintNFT)
import Network.HTTP.Req
import Plutus.Contract (ContractInstanceId)
import Plutus.PAB.Events.Contract (ContractInstanceId (unContractInstanceId))
import Text.Printf (printf)
import Utils (getBlockConfirmations)
import Wallet.Emulator.Wallet (WalletId (..))
import Prelude hiding (id)

txListener :: TxHash -> String -> Address -> IO (Either String TxHash)
txListener txHash tokenName sender = do
  printf "Listening to block confirmations on transaction %s\n" (show txHash)

  go txHash 30
  where
    go :: TxHash -> Integer -> IO (Either String TxHash)
    go txh tries = do
      -- Amount of required block confirmations we require before we mint and send NFT
      let goal = 1
      confirmationsE <- getBlockConfirmations txh
      case confirmationsE of
        Left _ -> do
          if tries > 0
            then do
              printf "Attempting to fetch block confirmations, retrying %s more times...\n" (show tries)
              threadDelay 4_000_000
              go txh (tries - 1)
            else do
              printf "Error fetching block confirmations, stopping.\n" (show tries)
              return $ Left "Error fetching block confirmations"
        Right confirmations -> do
          if confirmations >= goal
            then do
              printf "Success! We've reached %s confirmations\n" (show goal)
              -- We want to filter requests based on how recent they are so that we don't have to fetch
              -- all of the txs that the wallet has ever sent.
              -- The timestamp in the requests seem to be several seconds in the past, so passing a
              -- timestamp in the past to be sure most recent tx is included.
              -- recentTimeStamp <-
              --   iso8601Show . addUTCTime (secondsToNominalDiffTime (-60))
              --     <$> getCurrentTime
              timeNow <- iso8601Show <$> getCurrentTime
              printf "Calling `mintNFT` with tokenName: %s\n" (show tokenName)
              walletIdE <- mintNFT tokenName sender >>= checkContractStatus

              case walletIdE of
                Left e -> do
                  return $ Left ("Error confirming minting status: " <> show e)
                Right walletId -> do
                  let f = fetchTxs timeNow walletId
                      attempt :: Integer -> IO (Either String TxHash) -> IO (Either String TxHash)
                      attempt n io = do
                        resE <- io
                        case resE of
                          Left e -> do
                            if n > 0
                              then do
                                printf "Attempt failed, %d left, error: %s\n" n (show e)
                                threadDelay 5_000_000
                                attempt (n - 1) io
                              else do
                                io
                          Right a -> return $ Right a

                  txHashE <- attempt 20 f
                  case txHashE of
                    Left e -> do
                      printf ("Error" <> show e)
                      return $ Left e
                    Right a -> do
                      printf ("Success, tx hash: " <> show a)
                      return $ Right a
            else do
              printf
                "Current confirmations %s out of required %s before minting NFT\n"
                (show confirmations)
                (show goal)
              threadDelay 2_000_000
              go txh tries

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
