{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module PAB
  ( Address,
    TokenContracts (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi.Schema (ToSchema)
import GHC.Generics (Generic)
import Ledger (Address, PaymentPubKeyHash (..))
import qualified Monitor
import Plutus.PAB.Effects.Contract.Builtin (Empty, HasDefinitions (..), SomeBuiltin (..), endpointsToSchemas)
import Prettyprinter (Pretty (..), viaShow)
import qualified SendContract
import qualified Token.OffChain as Token
import Wallet.Emulator.Wallet (knownWallet, mockWalletAddress)

data TokenContracts
  = Mint Token.TokenParams
  | Monitor Address
  | Send SendContract.SendParams
  deriving (Eq, Show, Ord, Generic, FromJSON, ToJSON, ToSchema)

instance Pretty TokenContracts where
  pretty = viaShow

instance HasDefinitions TokenContracts where
  getDefinitions = [Mint exampleTP, Monitor exampleAddr, Send exampleSP]

  getContract (Mint tp) = SomeBuiltin $ Token.mintToken @() @Empty tp
  getContract (Monitor addr) = SomeBuiltin $ Monitor.monitor addr
  getContract (Send sp) = SomeBuiltin $ SendContract.send @() @Empty sp

  getSchema = const $ endpointsToSchemas @Empty

exampleAddr :: Address
exampleAddr = mockWalletAddress $ knownWallet 1

exampleTP :: Token.TokenParams
exampleTP =
  Token.TokenParams
    { Token.tpAddress = exampleAddr,
      Token.tpAmount = 123456,
      Token.tpToken = "PPP"
    }

wallet1Pkh :: Ledger.PaymentPubKeyHash
wallet1Pkh =
  PaymentPubKeyHash
    "fd5309253a86281ffbfee20e7c63b4b7d83bdcee309ba810d96639c4"

exampleSP :: SendContract.SendParams
exampleSP =
  SendContract.SendParams
    { SendContract.amount = 500,
      SendContract.pkh = wallet1Pkh
    }
