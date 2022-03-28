{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Spec.Model
  ( tests,
    AuctionModel (..),
  )
where

import Control.Lens hiding (elements)
import Control.Monad (void, when)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.String (IsString (..))
import Data.Text (Text)
import EnglishAuction hiding (Bid, Close)
import Ledger
  ( CurrencySymbol,
    minAdaTxOut,
  )
import qualified Ledger.Ada as Ada
import Ledger.Value
  ( AssetClass (AssetClass),
    TokenName (..),
    assetClass,
    assetClassValue,
    assetClassValueOf,
    unAssetClass,
  )
import Plutus.Contract
import Plutus.Contract.Test
  ( InitialDistribution,
    Wallet,
    defaultCheckOptions,
    emulatorConfig,
    w1,
    w2,
  )
import Plutus.Contract.Test.ContractModel as Test
import Plutus.Contract.Test.ContractModel.Symbolics (SymValue (actualValPart))
import Plutus.Contract.Trace (mockWalletPaymentPubKeyHash)
import Plutus.Trace.Emulator as Trace
import PlutusTx.Prelude (BuiltinByteString)
import Test.QuickCheck as QuickCheck
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)

data AuctionState = AuctionState
  { _asMinBid :: !Integer,
    _asHighestBid :: !(Maybe Integer),
    _asToken :: !(Maybe (AssetClass, Integer))
  }
  deriving (Show)

makeLenses ''AuctionState

newtype AuctionModel = AuctionModel {_auctionModel :: Map Wallet AuctionState}
  deriving (Show)

makeLenses ''AuctionModel

tests :: TestTree
tests = testProperty "english auction model" prop_Auction

instance ContractModel AuctionModel where
  data Action AuctionModel
    = Start Wallet StartParams
    | Bid Wallet Wallet BidParams -- First `Wallet` owns auction, second runs/interacts with contract
    | Close Wallet Wallet CloseParams
    deriving (Show, Eq)

  data ContractInstanceKey AuctionModel w s e p where
    StartKey ::
      Wallet ->
      ContractInstanceKey AuctionModel () AuctionStartSchema Text ()
    UseKey ::
      Wallet ->
      Wallet ->
      ContractInstanceKey AuctionModel () AuctionUseSchema Text ()

  -- Maybe necessary to have two because one starts a contract and the other one operates
  -- on a running contract?
  instanceWallet :: ContractInstanceKey AuctionModel w s e p -> Wallet
  instanceWallet (StartKey w) = w
  instanceWallet (UseKey _ w) = w

  -- I think this is optional. Used to keep track of different running instances.
  instanceTag ::
    SchemaConstraints w s e =>
    ContractInstanceKey AuctionModel w s e p ->
    ContractInstanceTag
  instanceTag key = fromString $ "instance tag for: " ++ show key

  arbitraryAction :: ModelState AuctionModel -> Gen (Action AuctionModel)
  arbitraryAction _ =
    oneof
      [ Start <$> genWallet <*> genStartParams,
        Bid <$> genWallet <*> genWallet <*> genBidParams,
        Close <$> genWallet <*> genWallet <*> genCloseParams
      ]

  initialState :: AuctionModel
  initialState = AuctionModel Map.empty

  initialInstances :: [StartContract AuctionModel]
  initialInstances =
    [StartContract (StartKey v) () | v <- wallets]
      ++ [StartContract (UseKey v w) () | v <- wallets, w <- wallets]

  -- Check if the state of a wallet is Just, if so we must've started
  precondition :: ModelState AuctionModel -> Action AuctionModel -> Bool
  precondition s (Start w _) = isNothing $ getAuctionState' s w
  precondition s (Bid v _ BidParams {bpSeller}) =
    isJust (getAuctionState' s v)
      && mockWalletPaymentPubKeyHash v == bpSeller
  precondition s (Close v _ CloseParams {cpSeller}) =
    isJust (getAuctionState' s v) && mockWalletPaymentPubKeyHash v == cpSeller

  nextState :: Action AuctionModel -> Spec AuctionModel ()
  nextState (Start w StartParams {spCurrency, spToken, spMinBid}) = do
    wait 3
    let token = assetClass spCurrency spToken
    (auctionModel . at w) $= Just (AuctionState spMinBid Nothing (Just (token, 1)))
    withdraw w $ assetClassValue token 1
    withdraw w $ Ada.toValue minAdaTxOut
  nextState (Bid v w BidParams {bpCurrency, bpToken, bpBid}) = do
    wait 3
    started <- hasStarted v
    when started $ do
      cs <- getAuctionState v
      bc <- actualValPart <$> askModelState (view $ balanceChange w)
      case cs of
        Just t -> do
          let highestBid = fromMaybe 0 (t ^. asHighestBid)
              minBid = t ^. asMinBid
              correctToken =
                maybe
                  False
                  (\(token, n) -> token == assetClass bpCurrency bpToken && n > 0)
                  (t ^. asToken)
              isAcceptableBid =
                bpBid > highestBid && bpBid > minBid && bpBid > minLovelace && correctToken
              hasFunds = assetClassValueOf bc (tokens Map.! v) > bpBid

          when (isAcceptableBid && hasFunds) $ do
            let bidValue = Ada.lovelaceValueOf bpBid
            withdraw w bidValue
            (auctionModel . ix v . asHighestBid) $= Just bpBid
        _ -> return ()
  nextState (Close v w CloseParams {cpCurrency, cpToken}) = do
    wait 3
    started <- hasStarted v
    when started $ do
      cs <- getAuctionState v
      case cs of
        Just t -> do
          let hasToken =
                maybe
                  False
                  (\(token, n) -> token == assetClass cpCurrency cpToken && n > 0)
                  (t ^. asToken)
              highestBidM = t ^. asHighestBid
              validUtxo = maybe True (>= minLovelace) highestBidM

          when (validUtxo && hasToken) $ do
            when (isJust highestBidM) $ do
              deposit v (Ada.lovelaceValueOf (fromMaybe 0 highestBidM))
              deposit w $ Ada.toValue minAdaTxOut
            when (isNothing highestBidM) $ do
              deposit v $ Ada.toValue minAdaTxOut
              deposit v (maybe mempty (uncurry assetClassValue) (t ^. asToken))

            (auctionModel . at v) $= Nothing
        _ -> return ()

  startInstances ::
    ModelState AuctionModel ->
    Action AuctionModel ->
    [StartContract AuctionModel]
  startInstances _ _ = []

  instanceContract ::
    (SymToken -> AssetClass) ->
    ContractInstanceKey AuctionModel w s e p ->
    p ->
    Contract w s e ()
  instanceContract _ (StartKey _) () = useStartEndpoint
  instanceContract _ (UseKey _ _) () = useEndpoints'

  perform :: HandleFun AuctionModel -> (SymToken -> AssetClass) -> ModelState AuctionModel -> Action AuctionModel -> SpecificationEmulatorTrace ()
  perform h _ m (Start v p) =
    withWait m $ callEndpoint @"start" (h $ StartKey v) p
  perform h _ m (Bid v w p) =
    withWait m $ callEndpoint @"bid" (h $ UseKey v w) p
  perform h _ m (Close v w p) =
    withWait m $ callEndpoint @"close" (h $ UseKey v w) p

withWait :: ModelState AuctionModel -> SpecificationEmulatorTrace () -> SpecificationEmulatorTrace ()
withWait m c = void $ c >> waitUntilSlot ((m ^. Test.currentSlot) + 3)

hasStarted :: Wallet -> Spec AuctionModel Bool
hasStarted v = isJust <$> getAuctionState v

deriving instance Eq (ContractInstanceKey AuctionModel w s e p)

deriving instance Show (ContractInstanceKey AuctionModel w s e p)

getAuctionState' :: ModelState AuctionModel -> Wallet -> Maybe AuctionState
getAuctionState' s v = s ^. contractState . auctionModel . at v

getAuctionState :: Wallet -> Spec AuctionModel (Maybe AuctionState)
getAuctionState v = do
  s <- getModelState
  return $ getAuctionState' s v

wallets :: [Wallet]
wallets = [w1, w2]

tokenCurrencies :: Map Wallet CurrencySymbol
tokenCurrencies = Map.fromList $ zip wallets ["aa", "bb"]

tokenNames :: Map Wallet TokenName
tokenNames = Map.fromList $ zip wallets ["A", "B"]

tokens :: Map Wallet AssetClass
tokens = Map.fromList [(w, AssetClass (tokenCurrencies Map.! w, tokenNames Map.! w)) | w <- wallets]

genWallet :: Gen Wallet
genWallet = elements wallets

genNonNeg :: Gen Integer
genNonNeg = getNonNegative <$> arbitrary

instance Arbitrary BuiltinByteString where
  arbitrary = arbitrary

genStartParams :: Gen StartParams
genStartParams =
  (uncurry <$> partialParams) <*> randomToken
  where
    partialParams =
      StartParams <$> ((* 1_000_000) <$> genNonNeg)
    randomToken = elements (unAssetClass <$> Map.elems tokens)

genBidParams :: Gen BidParams
genBidParams =
  (uncurry BidParams <$> randomToken) <*> genNonNeg <*> seller
  where
    randomToken = elements (unAssetClass <$> Map.elems tokens)
    seller = mockWalletPaymentPubKeyHash <$> genWallet

genCloseParams :: Gen CloseParams
genCloseParams =
  uncurry CloseParams <$> randomToken <*> seller
  where
    randomToken = elements (unAssetClass <$> Map.elems tokens)
    seller = mockWalletPaymentPubKeyHash <$> genWallet

tokenAmt :: Integer
tokenAmt = 1_000

prop_Auction :: Actions AuctionModel -> Property
prop_Auction =
  withMaxSuccess 100
    . propRunActionsWithOptions
      (defaultCheckOptions & emulatorConfig . initialChainState .~ Left d)
      defaultCoverageOptions
      (const $ pure True)
  where
    d :: InitialDistribution
    d =
      Map.fromList $
        [ ( w,
            Ada.lovelaceValueOf 1_000_000_000
              <> mconcat [assetClassValue t tokenAmt | t <- Map.elems tokens]
          )
          | w <- wallets
        ]
