module Main where

import Affjax as AX
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import AppM (AppM, Env, runAppM)
import Cardano.Wallet.Nami as Nami
import Control.Monad.Error.Class (class MonadError, class MonadThrow, try)
import Control.Monad.Reader (class MonadAsk)
import Data.Argonaut.Core as A
import Data.Argonaut.Decode (JsonDecodeError, decodeJson)
import Data.Array (head, last)
import Data.ArrayBuffer.Typed as ArrayBuffer
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Cardano (CardanoWasm, loadCardanoWasm)
import Data.Cardano.Address as Address
import Data.Cardano.Transaction as Transaction
import Data.Cardano.TransactionWitnessSet as TransactionWitnessSet
import Data.Either (Either(..), either, hush)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), maybe)
import Data.String.CodePoints (length)
import Effect (Effect)
import Effect.Aff (Aff, attempt, throwError)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (Error, error)
import Env (pabProxyUrl)
import Foreign.Object as FO
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.Encoding as Encoding
import Prelude (Unit, bind, const, discard, pure, show, unit, (#), ($), (<$>), (<<<), (<=), (<>), (=<<), (>>=))
import Web.HTML (window)
import Web.HTML.Window (alert)

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    -- Introducing App monad so that we can load the `@emurgo/cardano-serialization-lib-browser`
    -- library and use it through the Purescript `foreign` bridges created in `Data.Cardano` modules.
    cardanoWasm <- loadCardanoWasm
    _ <- runUI (H.hoist (runAppM cardanoWasm) (component cardanoWasm)) unit body
    pure unit

data RemoteData a e = Success a
                    | Error e
                    | Loading
                    | NotAsked

type State
  = { cardanoWasm :: CardanoWasm
    , tokenName :: String
    , cidM :: Maybe String
    , pendingTx :: RemoteData (Maybe String) Error
    }

data Action
  = Initialize
  | MintToken String
  | SetTokenName String

component :: forall query input output. CardanoWasm -> H.Component query input output AppM
component cardanoWasm = do
  H.mkComponent
    { initialState: const $ initialState cardanoWasm
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just Initialize
              }
    }

initialState :: CardanoWasm -> State
initialState cardanoWasm = do
  { cardanoWasm
  , tokenName: ""
  , cidM: Nothing
  , pendingTx: NotAsked
  }

statusField :: forall w i. RemoteData (Maybe String) Error -> HH.HTML w i
statusField rd =
  let
    statusText = case rd of
      Error e -> "Error minting token: " <> show e
      Success txh -> "View tx hash: " <> show txh
      Loading -> "Loading"
      NotAsked -> "Not asked"
  in
    HH.div_ [ HH.text statusText ]

render :: forall m. State -> H.ComponentHTML Action () m
render state = do
  HH.div_
    [ HH.h1_
        [ HH.text "Royal Mint" ]
    , HH.input
        [ HP.value state.tokenName
        , HE.onValueInput SetTokenName
        ]
    , HH.button
        [ HE.onClick \_ -> MintToken state.tokenName ]
        [ HH.text "Mint NFT for 5 ada" ]
    , statusField state.pendingTx
    ]

type WalletBalances
  = { w1 :: A.Json, w2 :: A.Json }

jsonToWb :: A.Json -> Either JsonDecodeError WalletBalances
jsonToWb = decodeJson

handleAction ::
  forall output m
   . MonadAsk Env m
  => MonadAff m
  => MonadError Error m
  => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    -- Wallet 1 id
    let wid = "8c8c14997236e9372520d26666fb581e9b639ccb"
    cid <- H.liftAff $ fetchContractInstanceId wid
    -- Trigger popup for user to accept enabling of Nami integration
    _ <- H.liftAff $ Nami.enable
    log
      ( "Found cid: " <> show cid)

    H.modify_ (_ { cidM = Just cid })

  MintToken tokenName -> do
    if (length tokenName <= 0) then do
      eff <- liftEffect $ window >>= (alert "Token string is empty!")
      pure eff
    else do
      log ("Name.isEnabled: " <> show Nami.isEnabled)
      { cidM } <- H.get

      -- Nami returns a base16 encoded cbor-serialized address
      -- We need to convert it into bech32 format (i.e. "addr...")
      hexAddresses <- H.liftAff $ Nami.getUsedAddresses
      let hexAddrM = head hexAddresses

      log ("cbor hex address" <> show hexAddrM)

      hexAddr <- maybe ((throwError <<< error) "No used Nami addresses found") pure hexAddrM

      addrE <- mkFromCbor hexAddr Address.fromBytes

      let addrM = hush $ Address.toBech32 <$> addrE
      log ("bech32 address" <> show addrM)

      let dataM = cidM >>= (\cid -> addrM >>= (\wa -> Just { cid, wa }))

      case dataM of
        Just ({ cid, wa }) -> do
          log ("Minting token: " <> tokenName)

          H.modify_ (_ { pendingTx = Loading } )

          partialCborTx <- H.liftAff $ fetchContractPartialTx cid
          makePaymentRes <-
            H.lift
              $ try
              $ (balanceSignAndSubmitTx partialCborTx)

          case makePaymentRes of
            Left e -> do
              H.modify_ (_ { pendingTx = Error e } )
              log $ show e
            Right txId -> do
              resE <- H.liftAff $ postPendingTx (show txId) tokenName (show wa)
              case resE of
                Left e -> do
                  log ("ERROR: " <> show e)
                  H.modify_ (_ { pendingTx = Error e } )
                Right res -> do
                  H.modify_ (_ { pendingTx = Success $ A.toString res } )

        Nothing -> do
          log ( "Something went wrong, either with wallet addresses or fetching contract instance" )

  SetTokenName tn -> do
    H.modify_ (_ { tokenName = tn })

newtype PendingTxData =
  PendingTxData { txHash :: String }

postPendingTx :: String -> String -> String -> Aff (Either Error A.Json)
postPendingTx txHash tokenName walletAddress = do
  log ("postPendingTx: " <> txHash)
  let payload = FO.empty
                # FO.insert "txHash" (A.fromString txHash)
                # FO.insert "tokenName" (A.fromString tokenName)
                # FO.insert "senderAddress" (A.fromString walletAddress)

  resE <- AX.post
            ResponseFormat.json "/pending-tx"
            (Just (RequestBody.json (A.fromObject payload)))

  case resE of
    Left e -> (pure <<< Left <<< error <<< AX.printError) e
    Right res -> pure $ Right res.body

fetchContractInstanceId
  :: forall m
   . MonadAff m
  => MonadThrow Error m
  => String
  -> m String
fetchContractInstanceId wid = do
  resE <-
    H.liftAff
      $ AX.request
        ( AX.defaultRequest
                { url = (pabProxyUrl <> "/proxy/api/contract/instances/wallet/" <> wid)
                , method = Left GET
                , responseFormat = ResponseFormat.json
                }
        )
  res <- either (throwError <<< error <<< AX.printError) pure resE
  let
    cidM =
      (\x ->
        FO.lookup "cicContract" x
          >>= A.toObject
          >>= FO.lookup "unContractInstanceId"
          >>= A.toString
      )
        =<< A.toObject
  -- TODO: I picked the last contract instance, assuming that was the most recent, but it's not.
  -- By the looks of things, they're inserted at random and there's no date string to go by.
        =<< last
        =<< A.toArray res.body
  maybe (throwError $ error "Could not find [cicContract] in instances of wallet")
    pure
    cidM

-- | Fetch a partial transaction to be signed by web wallet
fetchContractPartialTx
  :: forall m
   . MonadAff m
  => MonadThrow Error m
  => String
  -> m String
fetchContractPartialTx cid = do
  resE <-
    H.liftAff
      $ AX.request
        ( AX.defaultRequest
                { url = (pabProxyUrl <> "/proxy/api/contract/instance/" <> cid <> "/status")
                , method = Left GET
                , responseFormat = ResponseFormat.json
                }
        )
  res <- either (throwError <<< error <<< AX.printError) pure resE
  let
    partialTxCborM =
      (\y -> A.toObject y >>= \x -> FO.lookup "transaction" x >>= A.toString)
        =<< head
        =<< A.toArray
        =<< FO.lookup "cicYieldedExportTxs"
        =<< A.toObject res.body
  maybe (throwError $ error "Could not parse fields cicYieldedExportTxs.[transaction]")
    pure
    partialTxCborM

-- | Given a partial transaction encoded in CBOR, balance, sign and submit it
-- using the Nami wallet api.
balanceSignAndSubmitTx
  :: forall m
   . MonadAsk Env m
  => MonadAff m
  => MonadThrow Error m
  => String
  -> m String
balanceSignAndSubmitTx partialTxCbor = do
  balancedTxCbor <- H.liftAff $ Nami.balanceTx partialTxCbor
  log ("balanceSignAndSubmitTx partialTxCbor:" <> show partialTxCbor)
  balancedTxE <- mkFromCbor balancedTxCbor Transaction.fromBytes
  balancedTx <- either throwError pure balancedTxE
  txWitnessSetCborE <- H.liftAff $ attempt $ Nami.signTx balancedTxCbor Nothing
  txWitnessSetCbor <- either throwError pure txWitnessSetCborE
  txWitnessSetE <- mkFromCbor txWitnessSetCbor TransactionWitnessSet.fromBytes
  txWitnessSet <- either throwError pure txWitnessSetE
  finalTxE <- Transaction.new (Transaction.body balancedTx) txWitnessSet
  finalTx <- either throwError pure finalTxE
  finalTxCbor <- bytesToCbor $ Transaction.toBytes finalTx
  log ("balanceSignAndSubmitTx finalTxCbor:" <> show finalTxCbor)
  Nami.submitTx finalTxCbor

-- | Decode a CBOR string to the given type 'a'.
mkFromCbor
  :: forall m a
   . MonadEffect m
  => String
  -> (Uint8Array -> m (Either Error a))
  -> m (Either Error a)
mkFromCbor cbor mk = do
  bytes <-
    liftEffect $ (Buffer.fromString cbor Encoding.Hex :: Effect Buffer)
      >>= Buffer.toArrayBuffer
      >>= (\x -> ArrayBuffer.whole x :: Effect Uint8Array)
  mk bytes

-- | Convert a byte array to a CBOR string.
bytesToCbor
  :: forall m
   . MonadEffect m
  => Uint8Array
  -> m String
bytesToCbor bytes =
  liftEffect $ (Buffer.fromArrayBuffer (ArrayBuffer.buffer bytes) :: Effect Buffer)
    >>= Buffer.toString Encoding.Hex
