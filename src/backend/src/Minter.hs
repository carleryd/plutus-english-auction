{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Minter where

import Blockfrost.Types.Shared hiding (Address, Slot, unSlot)
import Contract.Utils
import Contracts (mintNFT)
import Data.Either.Extras (unsafeFromEither)
import Data.List
import Data.Text (pack)
import Endpoints
import GHC.Conc (threadDelay)
import Ledger.Address (Address)
import Ledger.Value (TokenName (TokenName))
import Text.Printf (printf)
import Utils (getBlockConfirmations)
import Wallet.Emulator.Wallet (WalletId)
import Prelude hiding (id)

-- | Check if user has paid us the cost of minting their NFT.
-- | If so, mint and send user the hash of that transaction.
mintOnConfirmation :: String -> String -> Address -> IO (Either String String)
mintOnConfirmation txHash tokenName sender = do
  printf "Listening to block confirmations on transaction %s\n" (show txHash)

  mintAttempt txHash 30
  where
    mintAttempt :: String -> Integer -> IO (Either String String)
    mintAttempt txh attemptsLeft = do
      -- Amount of required block confirmations we require before we mint and send NFT
      let goal = 0
      confirmationsE <- getBlockConfirmations $ (TxHash . pack) txh
      case confirmationsE of
        Left _ -> do
          if attemptsLeft > 0
            then do
              printf "Attempting to fetch block confirmations, retrying %s more times...\n" (show attemptsLeft)
              threadDelay 4_000_000
              mintAttempt txh (attemptsLeft - 1)
            else do
              printf "Error fetching block confirmations, stopping.\n" (show attemptsLeft)
              return $ Left "Error fetching block confirmations"
        Right confirmations -> do
          if confirmations >= goal
            then do
              printf "Success! We've reached %s confirmations\n" (show goal)

              printf "Calling `mintNFT` with tokenName: %s\n" (show tokenName)
              walletIdE <- mintNFT tokenName sender >>= checkContractStatus

              case walletIdE of
                Left e -> do
                  return $ Left ("Error confirming minting status: " <> show e)
                Right walletId -> do
                  return $ Right $ show walletId
            else do
              printf
                "Current confirmations %s out of required %s before minting NFT\n"
                (show confirmations)
                (show goal)
              threadDelay 2_000_000
              mintAttempt txh attemptsLeft

containsAsset :: [Transaction] -> String -> Either String String
containsAsset [] _ = Left "No assets here"
containsAsset (tx : txs) an =
  case find (\asset -> asset_name asset == an) (outputs tx >>= assets) of
    Nothing -> containsAsset txs an
    Just _ -> Right (id tx)

getMintTxHash :: WalletId -> String -> IO (Either String String)
getMintTxHash walletId tn = do
  let fetchAttempt :: Integer -> IO (Either String [Transaction]) -> IO (Either String String)
      fetchAttempt n fetchTxs = do
        resE <- fetchTxs
        case resE of
          Left e -> do
            if n > 0
              then do
                printf "fetchAttempt failed, %d left, error: %s\n" n (show e)
                threadDelay 5_000_000
                fetchAttempt (n - 1) fetchTxs
              else do
                return $ Left "Error getMintTxHash: ran out of attempts"
          Right a -> do
            txs <- unsafeFromEither <$> fetchTxs
            -- TODO: Replace with input from client
            case containsAsset txs (unsafeStringToHex tn) of
              Left _ -> fetchAttempt (n - 1) fetchTxs
              Right txHash -> do
                printf "Success! Minting tx: %s\n" txHash
                return $ Right txHash
  txHashE <- fetchAttempt 20 $ fetchMostRecentTx walletId
  case txHashE of
    Left e -> do
      printf "Error: %s" e
      return $ Left e
    Right a -> do
      printf "Success, tx hash: %s\n" a
      return $ Right a
