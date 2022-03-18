{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Control.Monad (when)
import Data.Aeson (FromJSON (..))
import Data.Aeson.Types (parseMaybe)
import Data.Maybe (fromMaybe)
import Data.Monoid (Last (..))
import Data.Text (pack)
import Network.HTTP.Req
import PAB (Address, TokenContracts (..))
import Plutus.PAB.Events.ContractInstanceState (PartiallyDecodedResponse (..))
import Plutus.PAB.Webserver.Types (ContractInstanceClientState (..))
import Plutus.V1.Ledger.Value (Value, flattenValue)
import System.Environment (getArgs)
import Text.Printf (printf)
import Utils (cidToString, contractActivationArgs, unsafeReadAddress, unsafeReadWalletId)
import Wallet.Emulator.Wallet (WalletId (..))
import Wallet.Types (ContractInstanceId (..))

address :: Address
address = unsafeReadAddress "addr_test1qrrswgu70f8wuhdqnk7tk52dla2c7peuukvlvmu0e338m4w5qqhj89csyv7y7543l24hz8t00rrgyct2p5gv3udgtpxqprzavs"

wid :: WalletId
wid = unsafeReadWalletId "8c8c14997236e9372520d26666fb581e9b639ccb"

getState :: IO (ContractInstanceClientState TokenContracts)
getState = do
  instanceId <- startMonitor wid address
  getMonitorState instanceId

getValue :: IO (Maybe Value)
getValue = observedValue <$> getState

main :: IO ()
main = do
  [wid', addr'] <- getArgs
  let wid = unsafeReadWalletId wid'
      addr = unsafeReadAddress addr'
  printf "monitoring address %s on wallet %s\n" (show addr) $ show wid
  cid <- startMonitor wid addr
  printf "started monitor-process with contract id %s\n\n" $ cidToString cid
  go cid mempty
  where
    go :: ContractInstanceId -> Value -> IO ()
    go cid v = do
      cic <- getMonitorState cid
      let v' = fromMaybe v $ observedValue cic
      -- when (v' /= v) $
      printf "%s\n\n" $ show $ flattenValue v'
      threadDelay 1_000_000
      go cid v'

startMonitor :: WalletId -> Address -> IO ContractInstanceId
startMonitor wid addr = do
  v <-
    runReq defaultHttpConfig $
      req
        POST
        (http "127.0.0.1" /: "api" /: "contract" /: "activate")
        (ReqBodyJson $ contractActivationArgs wid $ Monitor addr)
        jsonResponse
        (port 9080)
  let c = responseStatusCode v
  when (c /= 200) $
    throwIO $ userError $ printf "ERROR: %d\n" c
  return $ responseBody v

getMonitorState :: ContractInstanceId -> IO (ContractInstanceClientState TokenContracts)
getMonitorState cid = do
  v <-
    runReq defaultHttpConfig $
      req
        GET
        (http "127.0.0.1" /: "api" /: "contract" /: "instance" /: pack (cidToString cid) /: "status")
        NoReqBody
        jsonResponse
        (port 9080)
  let c = responseStatusCode v
  when (c /= 200) $
    throwIO $ userError $ printf "ERROR: %d\n" c
  return $ responseBody v

observedValue :: ContractInstanceClientState TokenContracts -> Maybe Value
observedValue cic = do
  Last mv <- parseMaybe parseJSON $ observableState $ cicCurrentState cic
  mv
