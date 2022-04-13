{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module EnglishAuction
  ( Auction (..),
    StartParams (..),
    BidParams (..),
    CloseParams (..),
    Bid (..),
    AuctionUseSchema,
    AuctionStartSchema,
    start,
    bid,
    close,
    useEndpoints,
    useEndpoints',
    useStartEndpoint,
    auctionValidator,
    AuctionDatum (..),
    AuctionAction (..),
    tsCovIdx,
    minLovelace,
  )
where

import Control.Monad (Monad (return, (>>)), forever, void, when)
import Data.Aeson (FromJSON, ToJSON)
import Data.Data
import Data.Default (Default (def))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map as Map (singleton, toList)
import Data.Maybe
import Data.Monoid (Last (..))
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Ledger
import Ledger.Ada as Ada (lovelaceValueOf)
import qualified Ledger.Constraints as Constraints
import Ledger.TimeSlot (slotToBeginPOSIXTime)
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Value as Value
import Plutus.Contract
import qualified PlutusTx
import PlutusTx.Code (getCovIdx)
import PlutusTx.Coverage (CoverageIndex)
import PlutusTx.Prelude
  ( AdditiveSemigroup ((+)),
    Bool (..),
    Either (Left, Right),
    Eq (..),
    Integer,
    Maybe (..),
    Ord ((<), (>=)),
    Semigroup ((<>)),
    max,
    otherwise,
    trace,
    traceError,
    traceIfFalse,
    traceIfTrue,
    ($),
    (&&),
    (.),
    (||),
  )
import Schema (ToSchema)
import Text.Printf (printf)
import qualified Prelude as Haskell

minLovelace :: Integer
minLovelace = 2000000

data Auction = Auction
  { aSeller :: !PaymentPubKeyHash,
    aMinBid :: !Integer,
    aCurrency :: !CurrencySymbol,
    aToken :: !TokenName
  }
  deriving (Haskell.Show, Generic, ToJSON, FromJSON, ToSchema)

instance Eq Auction where
  {-# INLINEABLE (==) #-}
  a == b =
    (aSeller a == aSeller b)
      && (aMinBid a == aMinBid b)
      && (aCurrency a == aCurrency b)
      && (aToken a == aToken b)

PlutusTx.unstableMakeIsData ''Auction
PlutusTx.makeLift ''Auction

data Bid = Bid
  { bBidder :: !PaymentPubKeyHash,
    bBid :: !Integer
  }
  deriving (Haskell.Show)

instance Eq Bid where
  {-# INLINEABLE (==) #-}
  b == c =
    (bBidder b == bBidder c)
      && (bBid b == bBid c)

PlutusTx.unstableMakeIsData ''Bid
PlutusTx.makeLift ''Bid

data AuctionAction = MkBid Bid | Close | Abort
  deriving (Haskell.Show)

PlutusTx.unstableMakeIsData ''AuctionAction
PlutusTx.makeLift ''AuctionAction

data AuctionDatum = AuctionDatum
  { adAuction :: !Auction,
    adHighestBid :: !(Maybe Bid)
  }
  deriving (Haskell.Show)

PlutusTx.unstableMakeIsData ''AuctionDatum
PlutusTx.makeLift ''AuctionDatum

data Auctioning

instance Scripts.ValidatorTypes Auctioning where
  type RedeemerType Auctioning = AuctionAction
  type DatumType Auctioning = AuctionDatum

{-# INLINEABLE minBid #-}
minBid :: AuctionDatum -> Integer
minBid AuctionDatum {adHighestBid, adAuction} =
  max minLovelace x
  where
    x = case adHighestBid of
      Nothing -> aMinBid adAuction
      Just Bid {..} -> bBid + 1

{-# INLINEABLE mkAuctionValidator #-}
mkAuctionValidator :: AuctionDatum -> AuctionAction -> ScriptContext -> Bool
mkAuctionValidator ad redeemer ctx =
  traceIfFalse "wrong input value" correctInputValue
    && case redeemer of
      MkBid b@Bid {..} ->
        traceIfFalse "bid too low" (sufficientBid bBid)
          && traceIfFalse "wrong output datum" (correctBidOutputDatum b)
          && traceIfFalse "wrong output value" (correctBidOutputValue bBid)
          && traceIfFalse "wrong refund" correctBidRefund
      Close ->
        case adHighestBid ad of
          Nothing ->
            traceIfFalse
              "expected seller to get token"
              ( getsValue (aSeller auction) $
                  tokenValue
                    <> Ada.lovelaceValueOf minLovelace
              )
          Just (Bid bBidder bBid) ->
            traceIfFalse
              "expected highest bidder to get token"
              (getsValue bBidder $ tokenValue <> Ada.lovelaceValueOf minLovelace)
              && traceIfFalse
                "expected seller to get highest bid"
                (getsValue (aSeller auction) $ Ada.lovelaceValueOf bBid)
      Abort -> traceIfTrue "### Redeemer == Abort ###" True
  where
    getsValue :: PaymentPubKeyHash -> Value -> Bool
    getsValue h v =
      let utxos =
            [ o'
              | o' <- txInfoOutputs info,
                txOutValue o' == v
            ]
          lengthPrint = case txInfoOutputs info of
            [] -> traceIfTrue "### length $ txInfoOutputs info == 0 ###" True
            [_] -> traceIfTrue "### length $ txInfoOutputs info == 1 ###" True
            [_, _] -> traceIfTrue "### length $ txInfoOutputs info == 2 ###" True
            [_, _, _] -> traceIfTrue "### length $ txInfoOutputs info == 3 ###" True
            [_, _, _, _] -> traceIfTrue "### length $ txInfoOutputs info == 4 ###" True
            _ -> traceIfTrue "### length $ txInfoOutputs info > 4 ###" True
       in lengthPrint && case utxos of
            [o] ->
              traceIfTrue
                "### getsValue utxos length == 1 ###"
                True
                && traceIfFalse
                  "### getsValue wrong address ###"
                  (txOutAddress o == pubKeyHashAddress h Nothing)
            [] -> traceIfFalse "### getsValue utxos empty ###" False
            _ -> traceIfFalse "### getsValue utxos length > 1 ###" False

    info :: TxInfo
    info = scriptContextTxInfo ctx

    input :: TxInInfo
    input =
      let isScriptInput i = case (txOutDatumHash . txInInfoResolved) i of
            Nothing -> False
            Just _ -> True
          xs = [i | i <- txInfoInputs info, isScriptInput i]
       in case xs of
            [i] -> i
            _ -> traceError "expected exactly one script input"

    inVal :: Value
    inVal = txOutValue . txInInfoResolved $ input

    auction :: Auction
    auction = adAuction ad

    tokenValue :: Value
    tokenValue = Value.singleton (aCurrency auction) (aToken auction) 1

    correctInputValue :: Bool
    correctInputValue =
      inVal == case adHighestBid ad of
        Nothing ->
          tokenValue <> Ada.lovelaceValueOf minLovelace
        Just (Bid _ bBid) ->
          tokenValue <> Ada.lovelaceValueOf (minLovelace + bBid)

    sufficientBid :: Integer -> Bool
    sufficientBid amount = amount >= minBid ad

    ownOutput :: TxOut
    outputDatum :: AuctionDatum
    (ownOutput, outputDatum) = case getContinuingOutputs ctx of
      [o] -> case txOutDatumHash o of
        Nothing -> traceError "wrong output type"
        Just h -> case findDatum h info of
          Nothing -> traceError "datum not found"
          Just (Datum d) -> case PlutusTx.fromBuiltinData d of
            Just ad' -> (o, ad')
            Nothing -> traceError "error decoding data"
      _ -> traceError "expected exactly one continuing output"

    correctBidOutputDatum :: Bid -> Bool
    correctBidOutputDatum b =
      (adAuction outputDatum == auction)
        && (adHighestBid outputDatum == Just b)

    correctBidOutputValue :: Integer -> Bool
    correctBidOutputValue amount =
      txOutValue ownOutput == tokenValue <> Ada.lovelaceValueOf (minLovelace + amount)

    correctBidRefund :: Bool
    correctBidRefund = case adHighestBid ad of
      Nothing -> True
      Just Bid {..} ->
        let os =
              [ o
                | o <- txInfoOutputs info,
                  txOutAddress o == pubKeyHashAddress bBidder Nothing
              ]
         in case os of
              [o] -> txOutValue o == Ada.lovelaceValueOf bBid
              _ -> traceError "expected exactly one refund output"

-- TODO: Couldn't get deadline logic to work with Model Checker. See [ISSUE].
-- deadline :: POSIXTime
-- deadline = slotToBeginPOSIXTime def (aDeadline auction)

-- correctBidSlotRange :: Bool
-- correctBidSlotRange = to deadline `contains` txInfoValidRange info

-- correctCloseSlotRange :: Bool
-- correctCloseSlotRange = from deadline `contains` txInfoValidRange info

typedAuctionValidator :: Scripts.TypedValidator Auctioning
typedAuctionValidator =
  Scripts.mkTypedValidator @Auctioning
    $$(PlutusTx.compile [||mkAuctionValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @AuctionDatum @AuctionAction

auctionValidator :: Validator
auctionValidator = Scripts.validatorScript typedAuctionValidator

auctionHash :: Ledger.ValidatorHash
auctionHash = Scripts.validatorHash typedAuctionValidator

auctionAddress :: Ledger.Address
auctionAddress = scriptHashAddress auctionHash

tsCovIdx :: CoverageIndex
tsCovIdx = getCovIdx $$(PlutusTx.compile [||mkAuctionValidator||])

data StartParams = StartParams
  { spMinBid :: !Integer,
    spCurrency :: !CurrencySymbol,
    spToken :: !TokenName
  }
  deriving (Haskell.Show, Haskell.Eq, Generic, Data, ToJSON, FromJSON, ToSchema)

data BidParams = BidParams
  { bpCurrency :: !CurrencySymbol,
    bpToken :: !TokenName,
    bpBid :: !Integer,
    bpSeller :: !PaymentPubKeyHash
  }
  deriving (Haskell.Show, Haskell.Eq, Generic, Data, ToJSON, FromJSON, ToSchema)

data CloseParams = CloseParams
  { cpCurrency :: !CurrencySymbol,
    cpToken :: !TokenName,
    cpSeller :: !PaymentPubKeyHash
  }
  deriving (Haskell.Show, Haskell.Eq, Generic, Data, ToJSON, FromJSON, ToSchema)

start :: AsContractError e => StartParams -> Contract w s e ()
start StartParams {..} = do
  pkh <- ownPaymentPubKeyHash
  let a =
        Auction
          { aSeller = pkh,
            aMinBid = spMinBid,
            aCurrency = spCurrency,
            aToken = spToken
          }
      d =
        AuctionDatum
          { adAuction = a,
            adHighestBid = Nothing
          }
      v = Value.singleton spCurrency spToken 1 <> Ada.lovelaceValueOf minLovelace
      tx = Constraints.mustPayToTheScript d v
  ledgerTx <- submitTxConstraints typedAuctionValidator tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @Haskell.String $ printf "started auction %s for token %s" (Haskell.show a) (Haskell.show v)

bid :: forall w s. BidParams -> Contract w s Text ()
bid BidParams {bpCurrency, bpToken, bpBid, bpSeller} = do
  (oref, o, d@AuctionDatum {..}) <- findAuction bpCurrency bpToken bpSeller
  logInfo @Haskell.String $ printf "found auction utxo with datum %s" (Haskell.show d)

  when (bpBid < minBid d) $
    throwError $ pack $ printf "bid lower than minimal bid %d" (minBid d)
  pkh <- ownPaymentPubKeyHash
  let b = Bid {bBidder = pkh, bBid = bpBid}
      d' = d {adHighestBid = Just b}
      v = Value.singleton bpCurrency bpToken 1 <> Ada.lovelaceValueOf (minLovelace + bpBid)
      r = Redeemer $ PlutusTx.toBuiltinData $ MkBid b

      lookups =
        Constraints.typedValidatorLookups typedAuctionValidator
          Haskell.<> Constraints.otherScript auctionValidator
          Haskell.<> Constraints.unspentOutputs (Map.singleton oref o)
      tx = case adHighestBid of
        Nothing ->
          Constraints.mustPayToTheScript d' v
            <> Constraints.mustSpendScriptOutput oref r
        Just Bid {..} ->
          Constraints.mustPayToTheScript d' v
            <> Constraints.mustPayToPubKey bBidder (Ada.lovelaceValueOf bBid)
            <> Constraints.mustSpendScriptOutput oref r
  ledgerTx <- submitTxConstraintsWith lookups tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @Haskell.String $
    printf
      "made bid of %d lovelace in auction %s for token (%s, %s)"
      bpBid
      (Haskell.show adAuction)
      (Haskell.show bpCurrency)
      (Haskell.show bpToken)

close :: forall w s. CloseParams -> Contract w s Text ()
close (CloseParams cpCurrency cpToken cpSeller) = do
  (oref, o, datum) <- findAuction cpCurrency cpToken cpSeller
  logInfo @Haskell.String $ printf "found auction utxo with datum %s" (Haskell.show datum)

  let t = Value.singleton cpCurrency cpToken 1
      r = Redeemer $ PlutusTx.toBuiltinData Close
      seller = aSeller $ adAuction datum

      lookups =
        Constraints.typedValidatorLookups typedAuctionValidator
          Haskell.<> Constraints.otherScript auctionValidator
          Haskell.<> Constraints.unspentOutputs (Map.singleton oref o)
      tx =
        case adHighestBid datum of
          Nothing ->
            Constraints.mustPayToPubKey seller (t <> Ada.lovelaceValueOf minLovelace)
              <> Constraints.mustSpendScriptOutput oref r
          Just (Bid bBidder bBid) ->
            Constraints.mustPayToPubKey bBidder (t <> Ada.lovelaceValueOf minLovelace)
              <> Constraints.mustPayToPubKey seller (Ada.lovelaceValueOf bBid)
              <> Constraints.mustSpendScriptOutput oref r

  ledgerTx <- submitTxConstraintsWith lookups tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @Haskell.String $
    printf
      "closed auction %s for token (%s, %s)"
      (Haskell.show $ adAuction datum)
      (Haskell.show cpCurrency)
      (Haskell.show cpToken)

findAuction ::
  CurrencySymbol ->
  TokenName ->
  PaymentPubKeyHash ->
  Contract w s Text (TxOutRef, ChainIndexTxOut, AuctionDatum)
findAuction cs tn seller = do
  utxos <- utxosAt $ scriptHashAddress auctionHash
  let xs =
        [ (oref, o)
          | (oref, o) <- Map.toList utxos,
            Value.valueOf (_ciTxOutValue o) cs tn == 1,
            case _ciTxOutDatum o of
              Left _ -> False
              Right (Datum e) -> case PlutusTx.fromBuiltinData e of
                Nothing -> False
                Just AuctionDatum {adAuction} -> aSeller adAuction == seller
        ]
  case xs of
    [(oref, o)] -> case _ciTxOutDatum o of
      Left _ -> throwError "datum missing"
      Right (Datum e) -> case PlutusTx.fromBuiltinData e of
        Nothing -> throwError "datum has wrong type"
        Just d@AuctionDatum {..}
          | aCurrency adAuction == cs && aToken adAuction == tn -> return (oref, o, d)
          | otherwise -> throwError "auction token missmatch"
    [] -> throwError "auction utxo not found"
    _ -> throwError "auction utxo length > 1"

type AuctionStartSchema =
  Endpoint "start" StartParams

useStartEndpoint' ::
  ( AsContractError e,
    HasEndpoint "start" StartParams s
  ) =>
  Contract w s e ()
useStartEndpoint' =
  forever $ awaitPromise start'
  where
    start' = endpoint @"start" $ start

useStartEndpoint :: Contract () AuctionStartSchema Text ()
useStartEndpoint = useStartEndpoint'

useEndpoints' ::
  ( HasEndpoint "bid" BidParams s,
    HasEndpoint "close" CloseParams s
  ) =>
  Contract () s Text ()
useEndpoints' =
  forever $
    handleError logError $
      awaitPromise (bid' `select` close')
  where
    bid' = endpoint @"bid" bid
    close' = endpoint @"close" close

type AuctionUseSchema =
  AuctionStartSchema
    .\/ Endpoint "bid" BidParams
    .\/ Endpoint "close" CloseParams

type AuctionSchema =
  AuctionStartSchema .\/ AuctionUseSchema

-- TODO: Can we do this instead? useEndpoints = useEndpoints' <> useStartEndpoint'
useEndpoints :: Contract () AuctionSchema Text ()
useEndpoints =
  forever $
    handleError logError $
      awaitPromise (start' `select` bid' `select` close')
  where
    start' = endpoint @"start" $ start
    bid' = endpoint @"bid" bid
    close' = endpoint @"close" close

-- myToken :: KnownCurrency
-- myToken = KnownCurrency (ValidatorHash "f") "Token" (TokenName "T" :| [])
