{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Contract.PAB (TokenContracts (..))
import Contract.SendContract (SendParams (..))
import Contract.Utils (contractActivationArgs, unsafeReadAddress, unsafeReadWalletId)
import Control.Exception (throwIO)
import Data.ByteString
import Data.String (IsString (..))
import Ledger (PaymentPubKeyHash (..), StakePubKeyHash (..))
import Ledger.Ada (lovelaceValueOf)
import Ledger.Address (Address, addressCredential)
import Ledger.Credential (Credential (..))
import Network.HTTP.Req
import System.Environment (getArgs)
import Text.Printf (printf)
import Wallet.Emulator.Wallet (WalletId (..))
import Wallet.Types (ContractInstanceId (..))

wallet1Address :: Address
wallet1Address = unsafeReadAddress "addr_test1qplfk3cw70jwlq7gya2m0mrx6fxujrf6gm9v9c556j75v5k5qqhj89csyv7y7543l24hz8t00rrgyct2p5gv3udgtpxqy637fp"

wid :: WalletId
wid = unsafeReadWalletId "8c8c14997236e9372520d26666fb581e9b639ccb"

wallet1Pkh :: IO [Ledger.PaymentPubKeyHash]
wallet1Pkh = case (addressCredential wallet1Address) of
  PubKeyCredential pkh -> ioReturn [PaymentPubKeyHash pkh]
  _ -> ioReturn []

ioReturn :: a -> IO a
ioReturn = return

main :: IO ()
main = do
  [wid', amt'] <- getArgs
  [pkh'] <- wallet1Pkh
  let wid = unsafeReadWalletId wid'
      tp =
        SendParams
          { amount = (read amt'),
            pkh = pkh'
          }
  printf "minting token for wallet id %s with parameters %s\n" (show wid) $ show tp
  cid <- sendAda wid tp
  printf "minted tokens, contract instance id: %s\n" $ show cid

sendToWallet1 :: IO ()
sendToWallet1 = do
  [pkh'] <- wallet1Pkh
  let sp =
        SendParams
          { amount = 5_000_000,
            pkh = pkh'
          }
  printf "Sending ada to wallet %s\n" $ show sp
  cid <- sendAda wid sp
  printf "Sent ada, contract instance id: %s\n" $ show cid

remotePort :: Int
remotePort = 9081

sendAda :: WalletId -> SendParams -> IO ContractInstanceId
sendAda wid sp = do
  v <-
    runReq defaultHttpConfig $
      req
        POST
        (http "127.0.0.1" /: "api" /: "contract" /: "activate")
        (ReqBodyJson $ contractActivationArgs wid $ Send sp)
        jsonResponse
        (port remotePort)
  let c = responseStatusCode v
  if c == 200
    then return $ responseBody v
    else throwIO $ userError $ printf "ERROR: %d\n" c
