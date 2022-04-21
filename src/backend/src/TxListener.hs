{-# LANGUAGE NumericUnderscores #-}

module TxListener where

import Blockfrost.Types.Shared
import GHC.Conc (threadDelay)
import MintToken (mintNFT)
import Text.Printf (printf)
import Utils (getBlockConfirmations)

txListener :: TxHash -> String -> IO ()
txListener txHash tokenName = do
  printf "Listening to block confirmations on transaction %s\n" (show txHash)

  go txHash 30
  where
    go :: TxHash -> Integer -> IO ()
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
        Right confirmations -> do
          if confirmations >= goal
            then do
              printf "Success! We've reached %s confirmations\n" (show goal)
              printf "Calling `mintNFT` with tokenName: %s\n" (show tokenName)
              mintNFT tokenName
              return ()
            else do
              printf
                "Current confirmations %s out of required %s before minting NFT\n"
                (show confirmations)
                (show goal)
              threadDelay 2_000_000
              go txh tries
