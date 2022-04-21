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

module Contract.SendContract
  ( send,
    SendParams (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi.Schema (ToSchema)
import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics (Generic)
import Ledger (PaymentPubKeyHash)
import Ledger.Ada (lovelaceValueOf)
import Ledger.Constraints (adjustUnbalancedTx, mustPayToPubKey)
import Plutus.Contract (Contract, logInfo, mkTxConstraints, yieldUnbalancedTx)

data SendParams = SendParams
  { amount :: !Integer,
    pkh :: !PaymentPubKeyHash
  }
  deriving (Eq, Show, Ord, Generic, FromJSON, ToJSON, ToSchema)

send :: SendParams -> Contract w s Text ()
send SendParams {amount, pkh} = do
  utx <- mkTxConstraints @Void mempty (mustPayToPubKey pkh (lovelaceValueOf amount))
  logInfo @String $ show utx
  yieldUnbalancedTx $ adjustUnbalancedTx utx
