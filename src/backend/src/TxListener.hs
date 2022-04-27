{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TxListener where

import Blockfrost.Types.Shared hiding (Address, Slot, unSlot)
import Contracts (mintNFT)
import Data.Time.Clock.POSIX
import Data.Time.Format.ISO8601 (iso8601Show)
import Endpoints
import GHC.Conc (threadDelay)
import Ledger.Address (Address)
import Text.Printf (printf)
import Utils (getBlockConfirmations)
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
