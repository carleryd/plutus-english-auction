{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Contract.Token.OffChain
  ( TokenParams (..),
    adjustAndSubmitWith,
    mintToken,
  )
where

import Contract.Token.OnChain
import Contract.Utils (getCredentials)
import Control.Monad hiding (fmap)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.OpenApi.Schema (ToSchema)
import Data.Text (Text, pack)
import Data.Void (Void)
import GHC.Generics (Generic)
import Ledger hiding (mint, singleton)
import Ledger.Constraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Value as Value
import Plutus.Contract as Contract
import Plutus.Contract.Wallet (getUnspentOutput)
import qualified PlutusTx
import PlutusTx.Prelude hiding (Monoid (mempty), Semigroup (..), unless)
import Text.Printf (printf)
import Prelude (Semigroup (..), Show (show), String)
import qualified Prelude

data TokenParams = TokenParams
  { tpToken :: !TokenName,
    tpAmount :: !Integer,
    tpAddress :: !Address
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Ord, Generic, FromJSON, ToJSON, ToSchema)

adjustAndSubmitWith ::
  ( PlutusTx.FromData (Scripts.DatumType a),
    PlutusTx.ToData (Scripts.RedeemerType a),
    PlutusTx.ToData (Scripts.DatumType a),
    AsContractError e
  ) =>
  ScriptLookups a ->
  TxConstraints (Scripts.RedeemerType a) (Scripts.DatumType a) ->
  Contract w s e CardanoTx
adjustAndSubmitWith lookups constraints = do
  unbalanced <- adjustUnbalancedTx <$> mkTxConstraints lookups constraints
  Contract.logDebug @String $ printf "unbalanced: %s" $ show unbalanced
  unsigned <- balanceTx unbalanced
  Contract.logDebug @String $ printf "balanced: %s" $ show unsigned
  signed <- submitBalancedTx unsigned
  Contract.logDebug @String $ printf "signed: %s" $ show signed
  return signed

mintToken :: TokenParams -> Contract w s Text ()
mintToken tp = do
  Contract.logDebug @String $ printf "[CONTRACT] started minting: %s" $ show tp
  let addr = tpAddress tp
  case getCredentials addr of
    Nothing -> Contract.throwError $ pack $ printf "[CONTRACT] expected pubkey address, but got %s" $ show addr
    Just (x, my) -> do
      Contract.logDebug @String $ printf "[CONTRACT] getCredentials Just %s, %s)" (show x) (show my)

      -- TODO: This `pkh` doesn't hold any UTXOs. PAB must be broken, how do we handle this?
      -- pkh <- Contract.ownPaymentPubKeyHash
      -- Contract.logDebug @String $ printf "[CONTRACT] pkh: %s" (show pkh)

      oref <- getUnspentOutput
      Contract.logDebug @String $ printf "[CONTRACT] getUnspentOutput %s" (show oref)
      oM <- Contract.unspentTxOutFromRef oref
      Contract.logDebug @String $ printf "[CONTRACT] unspentTxOutFromRef %s" (show oM)

      -- utxos <- utxosAt (pubKeyHashAddress pkh Nothing)
      -- Contract.logDebug @String $ printf "[CONTRACT] utxos: %s" (show utxos)

      case oM of
        Nothing -> Contract.logError @String "[CONTRACT] No UTXOs found at pkh"
        Just o -> do
          Contract.logDebug @String $ printf "[CONTRACT] picked UTxO at %s" (show oref)

          let tn = tpToken tp
              amt = tpAmount tp
              cs = tokenCurSymbol oref tn amt
              val = Value.singleton cs tn amt
              c = case my of
                Nothing -> Constraints.mustPayToPubKey x val
                Just y -> Constraints.mustPayToPubKeyAddress x y val
              lookups =
                Constraints.mintingPolicy (tokenPolicy oref tn amt)
                  <> Constraints.unspentOutputs (Map.singleton oref o)
              constraints =
                Constraints.mustMintValue val
                  <> Constraints.mustSpendPubKeyOutput oref
                  <> c

          void $ adjustAndSubmitWith @Void lookups constraints
          Contract.logInfo @String $ printf "[CONTRACT] Submitted minting tx of %s" (show val)
