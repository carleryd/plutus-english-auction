{-# LANGUAGE OverloadedStrings #-}

module Main where

import Endpoints
import Minter (mintOnConfirmation)
import Network.Wai.Middleware.Cors
import Utils
import Web.Scotty

startServer :: IO ()
startServer = do
  wb <- Utils.getBlockfrostUtxos

  let endpoints =
        baseEndpoints
          <> balancesEndpoint wb
          <> postPendingTx mintOnConfirmation

  scotty 3000 (middleware simpleCors <> endpoints)

main :: IO ()
main = startServer
