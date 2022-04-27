{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Either.Extras (unsafeFromEither)
import Endpoints
import Network.Wai.Middleware.Cors
import TxListener (txListener)
import Utils
import Web.Scotty

startServer :: IO ()
startServer = do
  wb <- Utils.getBlockfrostUtxos

  let endpoints =
        baseEndpoints
          <> balancesEndpoint wb
          <> postPendingTx txListener

  scotty 3000 (middleware simpleCors <> endpoints)

main :: IO ()
main = startServer
