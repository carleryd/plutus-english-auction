{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
    test,
    AuctionModel (..),
  )
where

import Control.Lens hiding (elements)
import Control.Monad (void)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid (Last (..))
import Data.String (IsString (..))
import Data.Text (Text)
import EnglishAuction hiding (Bid, Close)
import Ledger
  ( AssetClass,
    CurrencySymbol,
  )
import Ledger.Ada as Ada (lovelaceValueOf)
import Ledger.Value
  ( AssetClass (AssetClass),
    CurrencySymbol (CurrencySymbol),
    TokenName (..),
    assetClassValue,
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
import Plutus.Trace.Emulator as Trace
import PlutusTx.Prelude (BuiltinByteString)
import Spec.Trace (startParams, token)
import Test.QuickCheck as QuickCheck
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)

data AuctionState = AuctionState
  { _asHighestBid :: !(Maybe Integer),
    _asToken :: !Integer
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
    = Start Wallet
    | Bid Wallet Wallet BidParams -- First `Wallet` owns auction, second runs/interacts with contract
    | Close Wallet Wallet CloseParams
    deriving (Show, Eq)

  data ContractInstanceKey AuctionModel w s e p where
    StartKey ::
      Wallet ->
      ContractInstanceKey AuctionModel (Last Auction) AuctionStartSchema Text ()
    UseKey ::
      Wallet ->
      Wallet ->
      ContractInstanceKey AuctionModel () AuctionSchema Text ()

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
      [ Start <$> genWallet,
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
  precondition s (Start w) = isNothing $ getAuctionState' s w
  precondition s (Bid v _ _) = isJust $ getAuctionState' s v
  precondition s (Close v _ _) = isJust $ getAuctionState' s v

  nextState :: Action AuctionModel -> Spec AuctionModel ()
  nextState (Start w) = do
    wait 3
    (auctionModel . at w) $= Just (AuctionState Nothing 1)
  nextState (Bid v _ b) = do
    wait 3
    -- TODO: Check if bid is actually higher
    (auctionModel . ix v . asHighestBid) $= Just (bpBid b)
  nextState (Close v w p) = do
    wait 3
    (auctionModel . at v) $= Just (AuctionState Nothing 1)
    do
      m <- getAuctionState v
      case m of
        Just t
          | t ^. asToken > 0 && isJust (t ^. asHighestBid) -> do
            deposit v (lovelaceValueOf (fromMaybe 0 $ t ^. asHighestBid))
            deposit w (assetClassValue (tokens Map.! v) (t ^. asToken))
        _ -> return ()

  -- Maybe we need this now that we don't use "init" endpoint?
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
  perform h _ m (Start v) =
    withWait m $ callEndpoint @"start" (h $ StartKey v) startParams
  perform h _ m (Bid v w p) =
    withWait m $ callEndpoint @"bid" (h $ UseKey v w) p
  perform h _ m (Close v w p) =
    withWait m $ callEndpoint @"close" (h $ UseKey v w) p

withWait :: ModelState AuctionModel -> SpecificationEmulatorTrace () -> SpecificationEmulatorTrace ()
withWait m c = void $ c >> waitUntilSlot ((m ^. Test.currentSlot) + 3)

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

genBidParams :: Gen BidParams
genBidParams =
  BidParams
    <$> (fst <$> randomToken)
    <*> (snd <$> randomToken)
    <*> genNonNeg
  where
    randomToken = elements (unAssetClass <$> Map.elems tokens)

--     CloseParams
--       { cpCurrency = currencySymbol,
--         cpToken = tokenName
--       }
-- where
--   (currencySymbol, tokenName) = unAssetClass token

-- TODO: Use the oneOf instead of arbitrary, and have a limited set of currencySymbol, tokenName pairs to generate from
genCloseParams :: Gen CloseParams
genCloseParams =
  CloseParams
    <$> (fst <$> randomToken)
    <*> (snd <$> randomToken)
  where
    randomToken = elements (unAssetClass <$> Map.elems tokens)

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
            lovelaceValueOf 1_000_000_000
              <> mconcat [assetClassValue t tokenAmt | t <- Map.elems tokens]
          )
          | w <- wallets
        ]

test :: IO ()
test = quickCheck prop_Auction
