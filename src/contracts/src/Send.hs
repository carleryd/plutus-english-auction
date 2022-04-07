{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module SendContract
  ( send,
    SendParams (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi.Schema (ToSchema)
import Data.Void (Void)
import GHC.Generics (Generic)
import Ledger (PaymentPubKeyHash, StakePubKeyHash, Value)
import Ledger.Constraints (adjustUnbalancedTx, mustPayToPubKeyAddress)
import Plutus.Contract (Contract, ContractError, logInfo, mkTxConstraints, yieldUnbalancedTx)

data SendParams = SendParams
  { amount :: Value,
    pkh :: PaymentPubKeyHash,
    skh :: StakePubKeyHash
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

send :: SendParams -> Contract w s ContractError ()
send SendParams {amount, pkh, skh} = do
  utx <- mkTxConstraints @Void mempty (mustPayToPubKeyAddress pkh skh amount)
  logInfo @String $ show utx
  yieldUnbalancedTx $ adjustUnbalancedTx utx
