{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -g -fplugin-opt PlutusTx.Plugin:coverage-all #-}

module Spec.Trace where

import Control.Exception
import Control.Lens
import Control.Monad (void)
import Data.Default (Default (..))
import Data.IORef
import qualified Data.Map as Map
import EnglishAuction
import Ledger
import qualified Ledger.Ada as Ada
import Ledger.TimeSlot (slotToEndPOSIXTime)
import Ledger.Value as Value
import Plutus.Contract.Test
import Plutus.Contract.Test.Coverage
import Plutus.Contract.Test.Coverage.ReportCoverage (writeCoverageReport)
import Plutus.Trace.Emulator as Emulator
import PlutusTx.Prelude hiding (Semigroup (..), check, unless)
import System.Exit
import Test.Tasty
import qualified Test.Tasty.HUnit as HUnit
import Prelude (IO, Semigroup (..), Show (..), String, putStrLn)

-- This is the hash of the policy script
-- It is run when this is consumed to check who is allowed to mint etc
-- OBS: Definitely out of date, but useful as example
assetSymbol :: Ledger.CurrencySymbol
assetSymbol = "48dbfffab1d62765a090c305924a1df65f950a2f630f15d13345fd29"

assetToken :: Ledger.TokenName
assetToken = "TUI"

token :: AssetClass
token = AssetClass (assetSymbol, assetToken)

startParams :: StartParams
startParams =
  StartParams
    { spDeadline = slotToEndPOSIXTime def 10,
      spMinBid = 5_000_000,
      spCurrency = assetSymbol,
      spToken = assetToken
    }

startDatum :: AuctionDatum
startDatum =
  AuctionDatum
    { adAuction = auction,
      adHighestBid = Nothing
    }
  where
    auction =
      Auction
        { aSeller = mockWalletPaymentPubKeyHash w1,
          aDeadline = spDeadline startParams,
          aMinBid = spMinBid startParams,
          aCurrency = spCurrency startParams,
          aToken = spToken startParams
        }

tests :: TestTree
tests =
  checkPredicateOptions
    myOptions
    "token sale trace"
    myPredicate
    myTrace

testCoverage :: IO ()
testCoverage = do
  cref <- newCoverageRef
  e <-
    try $
      defaultMain $
        checkPredicateOptionsCoverage
          myOptions
          "token sale trace"
          cref
          myPredicate
          myTrace
  case e of
    Left (c :: ExitCode) -> do
      putStrLn $ "Tasty exited with: " ++ show c
      report <- readCoverageRef cref
      writeCoverageReport "EnglishAuctionTrace" tsCovIdx report
    Right () -> putStrLn $ "unexpected tasty result"

emCfg :: EmulatorConfig
emCfg = EmulatorConfig (Left $ Map.fromList [(knownWallet w, v) | w <- [1 .. 3]]) def def
  where
    v :: Value
    v = Ada.lovelaceValueOf 1_000_000_000 <> assetClassValue token 1000

myOptions :: CheckOptions
myOptions = defaultCheckOptions & emulatorConfig .~ emCfg

myPredicate :: TracePredicate
myPredicate =
  walletFundsChange
    w1
    ( Ada.lovelaceValueOf winningBid <> assetClassValue token (-1)
        <> negate minUtxoAda
    )
    .&&. walletFundsChange
      w2
      ( Ada.lovelaceValueOf (- winningBid) <> assetClassValue token 1
          <> minUtxoAda
      )
    .&&. walletFundsChange w3 (Ada.lovelaceValueOf 0)
  where
    minUtxoAda = Ada.lovelaceValueOf minLovelace
    winningBid = 20_000_000

myTrace :: Emulator.EmulatorTrace ()
myTrace = do
  h1 <- Emulator.activateContractWallet w1 useEndpoints
  h2 <- Emulator.activateContractWallet w2 useEndpoints
  h3 <- Emulator.activateContractWallet w3 useEndpoints

  callEndpoint @"start" h1 startParams

  void $ Emulator.waitNSlots 1

  callEndpoint @"bid" h2 $
    BidParams
      { bpCurrency = assetSymbol,
        bpToken = assetToken,
        bpBid = 10_000_000
      }
  void $ Emulator.waitNSlots 1

  callEndpoint @"bid" h3 $
    BidParams
      { bpCurrency = assetSymbol,
        bpToken = assetToken,
        bpBid = 15_000_000
      }

  void $ Emulator.waitNSlots 1

  callEndpoint @"bid" h2 $
    BidParams
      { bpCurrency = assetSymbol,
        bpToken = assetToken,
        bpBid = 20_000_000
      }

  void $ Emulator.waitUntilSlot 20

  callEndpoint @"close" h1 $
    CloseParams
      { cpCurrency = assetSymbol,
        cpToken = assetToken
      }

  void $ Emulator.waitNSlots 1

checkPredicateOptionsCoverage ::
  CheckOptions ->
  String ->
  CoverageRef ->
  TracePredicate ->
  EmulatorTrace () ->
  TestTree
checkPredicateOptionsCoverage options nm (CoverageRef ioref) predicate action =
  HUnit.testCaseSteps nm $ \step -> do
    checkPredicateInner options predicate action step (HUnit.assertBool nm) (\rep -> modifyIORef ioref (rep <>))

test :: IO ()
test = Emulator.runEmulatorTraceIO' def emCfg myTrace
