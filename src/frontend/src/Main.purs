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
import Data.Either (Either(..), either, hush, note)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Aff (Aff, attempt, throwError)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (Error, error)
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
import Prelude (Unit, bind, const, discard, pure, show, unit, (#), ($), (<$>), (<<<), (<>), (=<<), (>>=))

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    log "Before loadCardanoWasm"
    cardanoWasm <- loadCardanoWasm
    log "After loadCardanoWasm"
    _ <- runUI (H.hoist (runAppM cardanoWasm) (component cardanoWasm)) unit body
    pure unit

type State
  = { cardanoWasm :: CardanoWasm
    , tokenName :: String
    , cidM :: Maybe String
    }

data Action
  = Initialize
  | GetNamiBalance String
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
  }

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
        [ HE.onClick \_ -> GetNamiBalance state.tokenName ]
        [ HH.text "Mint NFT for 5 ada" ]
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
    log
      ( "Found cid: " <> show cid)
    H.modify_ (_ { cidM = Just cid })
  GetNamiBalance tokenName -> do
    log ("Name.isEnabled: " <> show Nami.isEnabled)

    { cidM } <- H.get
    hexAddresses <- H.liftAff $ Nami.getUsedAddresses
    let hexAddrM = head hexAddresses
        hexAddrE = note "Fail" hexAddrM
    log ("Hex address" <> show hexAddrM)

    hexAddr <- either (throwError <<< error) pure hexAddrE

    addrE <- Address.fromBytes hexAddr

    let addrM = hush $ Address.toBech32 <$> addrE
    log ("Bech32 address" <> show addrM)

    let dataM = cidM >>= (\cid -> addrM >>= (\wa -> Just { cid, wa }))

    case dataM of
      Just ({ cid, wa }) -> do
        log ("Minting token: " <> tokenName)
        partialCborTx <- H.liftAff $ fetchContractPartialTx cid
        makePaymentRes <-
          H.lift
            $ try
            $ (balanceSignAndSubmitTx partialCborTx)

        case makePaymentRes of
          Left err -> do
            log $ show err
          Right txId -> do
            log ("Submit success with txId " <> show txId)
            H.liftAff $ postPendingTx (show txId) tokenName (show wa)

      Nothing -> do
        log ( "Something went wrong, either with wallet addresses or fetching contract instance" )

  SetTokenName tn -> do
    H.modify_ (_ { tokenName = tn })

newtype PendingTxData =
  PendingTxData { txHash :: String }

postPendingTx :: String -> String -> String -> Aff Unit
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
    Left e -> (throwError <<< error <<< AX.printError) e
    Right res -> log (show $ A.toString res.body)

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
                { url = ("http://localhost:8010/proxy/api/contract/instances/wallet/" <> wid)
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
                { url = ("http://localhost:8010/proxy/api/contract/instance/" <> cid <> "/status")
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
  log ("0 balanceSignAndSubmitTx")
  balancedTxCbor <- H.liftAff $ Nami.balanceTx partialTxCbor
  log ("1 partialTxCbor:" <> show partialTxCbor)
  balancedTxE <- mkFromCbor balancedTxCbor Transaction.fromBytes

  log ("2")
  balancedTx <- either throwError pure balancedTxE

  log ("3")
  txWitnessSetCborE <- H.liftAff $ attempt $ Nami.signTx balancedTxCbor Nothing

  log ("4")
  txWitnessSetCbor <- either throwError pure txWitnessSetCborE

  log ("5 txWitnessSetCbor:" <> show txWitnessSetCbor)
  txWitnessSetE <- mkFromCbor txWitnessSetCbor TransactionWitnessSet.fromBytes

  log ("6")
  txWitnessSet <- either throwError pure txWitnessSetE

  log ("7")
  finalTxE <- Transaction.new (Transaction.body balancedTx) txWitnessSet

  log ("8")
  finalTx <- either throwError pure finalTxE

  log ("9")
  finalTxCbor <- bytesToCbor $ Transaction.toBytes finalTx

  log ("10 finalTxCbor:" <> show finalTxCbor)
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
