{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (throwIO)
import Data.ByteString
import Data.String (IsString (..))
import Ledger (PaymentPubKeyHash (..), StakePubKeyHash (..))
import Ledger.Ada (lovelaceValueOf)
import Ledger.Address (Address)
import Network.HTTP.Req
import PAB (TokenContracts (..))
import SendContract (SendParams (..))
import System.Environment (getArgs)
import Text.Printf (printf)
import Utils (contractActivationArgs, unsafeReadAddress, unsafeReadWalletId)
import Wallet.Emulator.Wallet (WalletId (..))
import Wallet.Types (ContractInstanceId (..))

namiAddress :: Address
namiAddress = unsafeReadAddress "addr_test1qz5gr43kn7nnzdmastvqdkx7kgr8s7qjnf0zm7tvyhj6prjr4fq3q7yn6tgmx08xxq390qyu7asdf2qlngrhghts8uysdfed6a"

wid :: WalletId
wid = unsafeReadWalletId "8c8c14997236e9372520d26666fb581e9b639ccb"

wallet1Pkh :: Ledger.PaymentPubKeyHash
wallet1Pkh =
  PaymentPubKeyHash
    "fd5309253a86281ffbfee20e7c63b4b7d83bdcee309ba810d96639c4"

namiWid :: WalletId
namiWid = unsafeReadWalletId "3345524abf6bbe1809449224b5972c41790b6cf2"

main :: IO ()
main = do
  [wid', amt'] <- getArgs
  let wid = unsafeReadWalletId wid'
      tp =
        SendParams
          { amount = (read amt'),
            pkh = wallet1Pkh
          }
  printf "minting token for wallet id %s with parameters %s\n" (show wid) $ show tp
  cid <- sendAda wid tp
  printf "minted tokens, contract instance id: %s\n" $ show cid

sendToWallet1 :: IO ()
sendToWallet1 = do
  let sp =
        SendParams
          { amount = 5_000_000,
            pkh = wallet1Pkh
          }
  -- printf "Sending ada from Nami wallet" (show wid) $ show sp
  cid <- sendAda wid sp
  printf "Sent ada, contract instance id: %s\n" $ show cid

sendAda :: WalletId -> SendParams -> IO ContractInstanceId
sendAda wid sp = do
  v <-
    runReq defaultHttpConfig $
      req
        POST
        (http "127.0.0.1" /: "api" /: "contract" /: "activate")
        (ReqBodyJson $ contractActivationArgs wid $ Send sp)
        jsonResponse
        (port 9080)
  let c = responseStatusCode v
  if c == 200
    then return $ responseBody v
    else throwIO $ userError $ printf "ERROR: %d\n" c
