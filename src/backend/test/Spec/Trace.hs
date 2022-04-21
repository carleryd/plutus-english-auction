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

import Contract.EnglishAuction
import Control.Exception
import Control.Lens
import Control.Monad (void)
import Control.Monad.Freer.Extras as Extras
import Data.Default (Default (..))
import Data.IORef
import qualified Data.Map as Map
import Ledger
import qualified Ledger.Ada as Ada
import Ledger.TimeSlot (slotToBeginPOSIXTime)
import Ledger.Value (AssetClass (unAssetClass))
import Ledger.Value as Value
import Plutus.Contract as Contract
import Plutus.Contract.Test
import Plutus.Contract.Test.Coverage
import Plutus.Contract.Test.Coverage.ReportCoverage (writeCoverageReport)
import Plutus.Trace.Emulator as Emulator
import PlutusTx.Prelude hiding (Semigroup (..), check, unless)
import System.Exit
import Test.Tasty
import qualified Test.Tasty.HUnit as HUnit
import Prelude (IO, Semigroup (..), Show (..), String, putStrLn)

tokenA :: AssetClass
tokenA = AssetClass ("aa", "A")

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
emCfg = EmulatorConfig (Left $ Map.fromList [(knownWallet w, v) | w <- [1 .. 3]]) def
  where
    v :: Value
    v = Ada.lovelaceValueOf 1_000_000_000 <> assetClassValue tokenA 1000

myOptions :: CheckOptions
myOptions = defaultCheckOptions & emulatorConfig .~ emCfg

test1 :: TestTree
test1 =
  checkPredicateOptions
    myOptions
    "token sale trace"
    myPredicate
    myTrace

myPredicate :: TracePredicate
myPredicate =
  walletFundsChange
    w1
    ( Ada.lovelaceValueOf winningBid <> assetClassValue tokenA (-1)
        <> negate minUtxoAda
    )
    .&&. walletFundsChange
      w2
      ( Ada.lovelaceValueOf (- winningBid) <> assetClassValue tokenA 1
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

  let (currency, token) = unAssetClass tokenA

  callEndpoint @"start"
    h1
    StartParams
      { spMinBid = 5_000_000,
        spCurrency = currency,
        spToken = token
      }

  void $ Emulator.waitNSlots 1

  callEndpoint @"bid" h2 $
    BidParams
      { bpCurrency = "aa",
        bpToken = "A",
        bpBid = 10_000_000,
        bpSeller = mockWalletPaymentPubKeyHash w1
      }
  void $ Emulator.waitNSlots 1

  callEndpoint @"bid" h3 $
    BidParams
      { bpCurrency = "aa",
        bpToken = "A",
        bpBid = 15_000_000,
        bpSeller = mockWalletPaymentPubKeyHash w1
      }

  void $ Emulator.waitNSlots 1

  callEndpoint @"bid" h2 $
    BidParams
      { bpCurrency = "aa",
        bpToken = "A",
        bpBid = 20_000_000,
        bpSeller = mockWalletPaymentPubKeyHash w1
      }

  void $ Emulator.waitUntilSlot 20

  callEndpoint @"close" h1 $
    CloseParams
      { cpCurrency = "aa",
        cpToken = "A",
        cpSeller = mockWalletPaymentPubKeyHash w1
      }

  void $ Emulator.waitNSlots 1

test2 :: TestTree
test2 =
  checkPredicateOptions
    myOptions
    "token sale trace"
    myPredicate2
    myTrace2

myPredicate2 :: TracePredicate
myPredicate2 =
  walletFundsChange
    w1
    (negate (assetClassValue tokenA 1) <> negate minUtxoAda)
    .&&. walletFundsChange
      w2
      ( negate (assetClassValue tokenA 1)
          <> negate minUtxoAda
          <> negate (Ada.lovelaceValueOf 20_000_000)
          <> Ada.lovelaceValueOf 2_000_000
      )
    .&&. walletFundsChange
      w3
      ( negate (Ada.lovelaceValueOf 2_000_000)
          <> assetClassValue tokenA 1
          <> minUtxoAda
      )
  where
    minUtxoAda = Ada.lovelaceValueOf minLovelace

myTrace2 :: Emulator.EmulatorTrace ()
myTrace2 = do
  h1 <- Emulator.activateContractWallet w1 useEndpoints
  h2 <- Emulator.activateContractWallet w2 useEndpoints
  h3 <- Emulator.activateContractWallet w3 useEndpoints

  let (currency, token) = unAssetClass tokenA

  callEndpoint @"start"
    h1
    StartParams
      { spMinBid = 0,
        spCurrency = currency,
        spToken = token
      }

  void $ Emulator.waitNSlots 3

  callEndpoint @"start"
    h2
    StartParams
      { spMinBid = 0,
        spCurrency = currency,
        spToken = token
      }
  void $ Emulator.waitNSlots 3

  callEndpoint @"bid"
    h3
    BidParams
      { bpCurrency = currency,
        bpToken = token,
        bpBid = 2_000_000,
        bpSeller = mockWalletPaymentPubKeyHash w2
      }

  void $ Emulator.waitNSlots 1

  callEndpoint @"bid"
    h2
    BidParams
      { bpCurrency = currency,
        bpToken = token,
        bpBid = 20_000_000,
        bpSeller = mockWalletPaymentPubKeyHash w1
      }

  void $ Emulator.waitUntilSlot 20

  Extras.logInfo $ show "### WALLET 1: " <> show (mockWalletPaymentPubKeyHash w1)
  Extras.logInfo $ show "### WALLET 2: " <> show (mockWalletPaymentPubKeyHash w2)

  callEndpoint @"close"
    h1
    CloseParams
      { cpCurrency = currency,
        cpToken = token,
        cpSeller = mockWalletPaymentPubKeyHash w2
      }

  void $ Emulator.waitUntilSlot 11

test3 :: TestTree
test3 =
  checkPredicateOptions
    myOptions
    "token sale trace"
    myPredicate3
    myTrace3

myBid31 = 100_000

myBid32 = 200_000

myPredicate3 :: TracePredicate
myPredicate3 =
  walletFundsChange w1 (Ada.lovelaceValueOf 0)
    .&&. walletFundsChange w2 (Ada.lovelaceValueOf 0)
    .&&. walletFundsChange w3 (Ada.lovelaceValueOf 0)
  where
    minUtxoAda = Ada.lovelaceValueOf minLovelace

myTrace3 :: Emulator.EmulatorTrace ()
myTrace3 = do
  h1 <- Emulator.activateContractWallet w1 useEndpoints
  h2 <- Emulator.activateContractWallet w2 useEndpoints
  h3 <- Emulator.activateContractWallet w3 useEndpoints

  let (currency, token) = unAssetClass tokenA

  callEndpoint @"start"
    h2
    StartParams
      { spMinBid = 0,
        spCurrency = currency,
        spToken = token
      }

  void $ Emulator.waitNSlots 3

  callEndpoint @"close"
    h1
    CloseParams
      { cpCurrency = currency,
        cpToken = token,
        cpSeller = mockWalletPaymentPubKeyHash w2
      }

  void $ Emulator.waitNSlots 1

  Extras.logInfo $ show "### WALLET 1: " <> show (mockWalletPaymentPubKeyHash w1)
  Extras.logInfo $ show "### WALLET 2: " <> show (mockWalletPaymentPubKeyHash w2)
  Extras.logInfo $ show "### START TIME: " <> show (slotToBeginPOSIXTime def 0)

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

tests :: [TestTree]
tests = [test1, test2, test3]
