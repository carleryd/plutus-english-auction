{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Deploy where

import Cardano.Api
  ( FileError,
    PlutusScript,
    PlutusScriptV1,
    ScriptData (..),
    ScriptDataJsonSchema (ScriptDataJsonDetailedSchema),
    scriptDataToJson,
    writeFileTextEnvelope,
  )
import Cardano.Api.Shelley (PlutusScript (..))
import Codec.Serialise (serialise)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Data.Default (Default (..))
import EnglishAuction
import qualified Ledger
import Ledger.TimeSlot (slotToEndPOSIXTime)
import qualified Ledger.Value as Value
import PlutusTx (Data (..))
import qualified PlutusTx
import PlutusTx.Prelude (BuiltinByteString)

-- ABOUT
-- This can be used to publish our contract on the testnet or mainnet using `cardano-cli`
-- It could use a lot of work to become useful to anyone else.
-- I've abondoned this logic in favor of using the PAB.

wallet1Pkh :: Ledger.PaymentPubKeyHash
wallet1Pkh =
  Ledger.PaymentPubKeyHash
    "fd5309253a86281ffbfee20e7c63b4b7d83bdcee309ba810d96639c4"

wallet1 :: Ledger.Address
wallet1 =
  Ledger.pubKeyHashAddress wallet1Pkh Nothing

wallet2Pkh :: Ledger.PaymentPubKeyHash
wallet2Pkh =
  Ledger.PaymentPubKeyHash
    "170d11cb471e063f5809ba12eef355b49f207658aad9bfd381f5c30d"

wallet2 :: Ledger.Address
wallet2 =
  Ledger.pubKeyHashAddress wallet1Pkh Nothing

dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs) = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (List xs) = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n) = ScriptDataNumber n
dataToScriptData (B bs) = ScriptDataBytes bs

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file =
  LBS.writeFile file
    . encode
    . scriptDataToJson ScriptDataJsonDetailedSchema
    . dataToScriptData
    . PlutusTx.toData

writeValidator :: FilePath -> Ledger.Validator -> IO (Either (FileError ()) ())
writeValidator file =
  writeFileTextEnvelope
    @(PlutusScript PlutusScriptV1)
    file
    Nothing
    . PlutusScriptSerialised
    . SBS.toShort
    . LBS.toStrict
    . serialise
    . Ledger.unValidatorScript

outPath :: String
outPath = "/Users/jackjazz/Programming/crypto/cardano-node-1.33.0-macos/scripts/auction/"

-- This is the hash of the policy script
-- It is run when this is consumed to check who is allowed to mint etc
policyId :: Ledger.CurrencySymbol
policyId = "48dbfffab1d62765a090c305924a1df65f950a2f630f15d13345fd29"

assetSymbol :: Ledger.CurrencySymbol
assetSymbol = policyId

tuiAssetName :: BuiltinByteString
tuiAssetName = "TUI"

assetToken :: Ledger.TokenName
assetToken = Value.TokenName tuiAssetName

startParams :: StartParams
startParams =
  StartParams
    { spMinBid = 5 * 1000000,
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
        { aSeller = wallet1Pkh,
          aMinBid = spMinBid startParams,
          aCurrency = spCurrency startParams,
          aToken = spToken startParams
        }

setDatum :: AuctionDatum -> Bid -> AuctionDatum
setDatum oldDatum newBid = oldDatum {adHighestBid = Just newBid}

deployTokenValue :: Ledger.Value
deployTokenValue = Value.singleton (aCurrency auction) (aToken auction) 1
  where
    auction = adAuction startDatum

writeStartDatum :: IO ()
writeStartDatum = do
  print startDatum
  writeJSON
    (outPath ++ "data/start-datum.json")
    startDatum

closeRedeemer :: Ledger.Redeemer
closeRedeemer = Ledger.Redeemer $ PlutusTx.toBuiltinData Close

writeCloseRedeemer :: IO ()
writeCloseRedeemer =
  writeJSON
    (outPath ++ "data/close-redeemer.json")
    closeRedeemer

abortRedeemer :: Ledger.Redeemer
abortRedeemer = Ledger.Redeemer $ PlutusTx.toBuiltinData Abort

writeAbortRedeemer :: IO ()
writeAbortRedeemer =
  writeJSON
    (outPath ++ "data/abort-redeemer.json")
    abortRedeemer

mkBidRedeemer :: Bid -> Ledger.Redeemer
mkBidRedeemer b = Ledger.Redeemer $ PlutusTx.toBuiltinData $ MkBid b

makeBid :: Ledger.PaymentPubKeyHash -> BidParams -> Bid
makeBid pkh bp = Bid {bBidder = pkh, bBid = bpBid bp}

customBid :: Bid
customBid =
  makeBid
    wallet2Pkh
    BidParams
      { bpCurrency = assetSymbol,
        bpToken = assetToken,
        bpBid = 10 * 1000000,
        bpSeller = wallet1Pkh
      }

bidRedeemer :: Ledger.Redeemer
bidRedeemer = mkBidRedeemer customBid

writeBidRedeemer :: IO ()
writeBidRedeemer =
  writeJSON
    (outPath ++ "data/bid-w2-redeemer.json")
    bidRedeemer

newDatum :: AuctionDatum
newDatum = setDatum startDatum customBid

writeBidDatum :: IO ()
writeBidDatum =
  writeJSON
    (outPath ++ "data/bid-w2-datum.json")
    newDatum

buildBid :: IO ()
buildBid = do
  writeBidRedeemer
  writeBidDatum

writeAuctionValidator :: IO (Either (FileError ()) ())
writeAuctionValidator =
  writeValidator
    (outPath ++ "validators/auction.plutus")
    auctionValidator

buildAuction :: IO (Either (FileError ()) ())
buildAuction = do
  writeStartDatum
  writeCloseRedeemer
  writeAbortRedeemer
  writeAuctionValidator

-- Run ./scripts/auction/build.sh
-- Check ./scripts/balances.sh
-- Update and run ./scripts/auction/start.sh
-- Check ./scripts/balances.sh
-- Update and run ./scripts/auction/close.sh
