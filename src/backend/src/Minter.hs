{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Minter where

import Blockfrost.Types.Shared hiding (Address, Slot, unSlot)
import Contracts (mintNFT)
import Data.Text (pack)
import Data.Time.Clock.POSIX
import Data.Time.Format.ISO8601 (iso8601Show)
import Endpoints (checkContractStatus, fetchMintTx)
import GHC.Conc (threadDelay)
import Ledger.Address (Address)
import Text.Printf (printf)
import Utils (getBlockConfirmations)
import Prelude hiding (id)

-- | Check if user has paid us the cost of minting their NFT.
-- | If so, mint and send user the hash of that transaction.
mintOnConfirmation :: String -> String -> Address -> IO (Either String String)
mintOnConfirmation txHash tokenName sender = do
  printf "Listening to block confirmations on transaction %s\n" (show txHash)

  mintAttempt txHash 30
  where
    mintAttempt :: String -> Integer -> IO (Either String String)
    mintAttempt txh tries = do
      -- Amount of required block confirmations we require before we mint and send NFT
      let goal = 1
      confirmationsE <- getBlockConfirmations $ (TxHash . pack) txh
      case confirmationsE of
        Left _ -> do
          if tries > 0
            then do
              printf "Attempting to fetch block confirmations, retrying %s more times...\n" (show tries)
              threadDelay 4_000_000
              mintAttempt txh (tries - 1)
            else do
              printf "Error fetching block confirmations, stopping.\n" (show tries)
              return $ Left "Error fetching block confirmations"
        Right confirmations -> do
          if confirmations >= goal
            then do
              printf "Success! We've reached %s confirmations\n" (show goal)

              -- We want to filter requests based on how recent they are so that we don't have to fetch
              -- all of the txs that the wallet has ever sent.
              timeNow <- iso8601Show <$> getCurrentTime
              printf "Calling `mintNFT` with tokenName: %s\n" (show tokenName)
              walletIdE <- mintNFT tokenName sender >>= checkContractStatus

              case walletIdE of
                Left e -> do
                  return $ Left ("Error confirming minting status: " <> show e)
                Right walletId -> do
                  let fetchAttempt :: Integer -> IO (Either String String) -> IO (Either String String)
                      fetchAttempt n io = do
                        resE <- io
                        case resE of
                          Left e -> do
                            if n > 0
                              then do
                                printf "fetchAttempt failed, %d left, error: %s\n" n (show e)
                                threadDelay 5_000_000
                                fetchAttempt (n - 1) io
                              else do
                                io
                          Right a -> return $ Right a

                  txHashE <- fetchAttempt 20 $ fetchMintTx timeNow walletId
                  case txHashE of
                    Left e -> do
                      printf ("Error" <> e)
                      return $ Left e
                    Right a -> do
                      printf ("Success, tx hash: " <> a)
                      return $ Right a
            else do
              printf
                "Current confirmations %s out of required %s before minting NFT\n"
                (show confirmations)
                (show goal)
              threadDelay 2_000_000
              mintAttempt txh tries
