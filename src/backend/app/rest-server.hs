{-# LANGUAGE OverloadedStrings #-}

module Main where

import Endpoints
import Minter (getMintTxHash, mintOnConfirmation)
import Network.Wai.Middleware.Cors
import Web.Scotty

startServer :: IO ()
startServer = do
  let endpoints =
        baseEndpoints
          <> postPendingTx mintOnConfirmation
          <> getLastTx getMintTxHash

  scotty 3000 (middleware simpleCors <> endpoints)

main :: IO ()
main = startServer
