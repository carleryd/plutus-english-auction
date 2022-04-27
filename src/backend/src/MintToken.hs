{-# LANGUAGE OverloadedStrings #-}

module MintToken where

import Contract.PAB (TokenContracts (..))
import Contract.Token.OffChain (TokenParams (..))
import Contract.Utils (contractActivationArgs, unsafeReadAddress, unsafeReadWalletId)
import Control.Exception (throwIO)
import Data.String (IsString (..))
import Ledger.Address (Address)
import Network.HTTP.Req
import System.Environment (getArgs)
import Text.Printf (printf)
import Wallet.Emulator.Wallet (WalletId (..))
import Wallet.Types (ContractInstanceId (..))

namiAddress :: Address
namiAddress = unsafeReadAddress "addr_test1qz5gr43kn7nnzdmastvqdkx7kgr8s7qjnf0zm7tvyhj6prjr4fq3q7yn6tgmx08xxq390qyu7asdf2qlngrhghts8uysdfed6a"

-- Wallet 1 id
walletId :: WalletId
walletId = unsafeReadWalletId "8c8c14997236e9372520d26666fb581e9b639ccb"

main :: IO ()
main = do
  [amt', tn', wid', addr'] <- getArgs
  let wid = unsafeReadWalletId wid'
      tp =
        TokenParams
          { tpToken = fromString tn',
            tpAmount = read amt',
            tpAddress = unsafeReadAddress addr'
          }
  printf "minting token for wallet id %s with parameters %s\n" (show wid) $ show tp
  cid <- mintToken wid tp
  printf "minted tokens, contract instance id: %s\n" $ show cid

mintStatic :: IO ()
mintStatic = do
  let amt' = "500"
      tn' = "REWARD"
      wid' = walletId
      addr' = namiAddress
      tp =
        TokenParams
          { tpToken = fromString tn',
            tpAmount = read amt',
            tpAddress = addr'
          }

  printf "minting token for wallet id %s with parameters %s\n" (show walletId) $ show tp
  cid <- mintToken wid' tp
  printf "minted tokens, contract instance id: %s\n" $ show cid

mintNFT :: String -> Address -> IO ContractInstanceId
mintNFT tokenName sender = do
  let amt' = "1"
      wid' = walletId
      addr' = sender
      tp =
        TokenParams
          { tpToken = fromString tokenName,
            tpAmount = read amt',
            tpAddress = addr'
          }

  printf "minting token using wallet id %s with parameters %s\n" (show wid') $ show tp
  cid <- mintToken wid' tp
  printf "minted tokens, contract instance id: %s\n" $ show cid
  return cid

mintToken :: WalletId -> TokenParams -> IO ContractInstanceId
mintToken wid tp = do
  v <-
    runReq defaultHttpConfig $
      req
        POST
        (http "127.0.0.1" /: "api" /: "contract" /: "activate")
        (ReqBodyJson $ contractActivationArgs wid $ Mint tp)
        jsonResponse
        (port 9080)
  let c = responseStatusCode v
  if c == 200
    then return $ responseBody v
    else throwIO $ userError $ printf "ERROR: %d\n" c
