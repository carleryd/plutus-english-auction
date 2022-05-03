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
import Data.Array (fromFoldable, head, last)
import Data.ArrayBuffer.Typed as ArrayBuffer
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Cardano (CardanoWasm, loadCardanoWasm)
import Data.Cardano.Address as Address
import Data.Cardano.Transaction as Transaction
import Data.Cardano.TransactionWitnessSet as TransactionWitnessSet
import Data.Either (Either(..), either, hush)
import Data.HTTP.Method (Method(..))
import Data.List.Lazy (replicate)
import Data.Maybe (Maybe(..), maybe)
import Data.String.CodePoints (length)
import Data.String.Common (toUpper)
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
import Prelude (Unit, bind, const, discard, pure, show, unit, (#), ($), (<$>), (<<<), (<=), (<>), (=<<), (>>=))
import Web.HTML (window)
import Web.HTML.Window (alert)

-- | I get annoyed writing `class_ $ ClassName "..."` over and over again. This small utility saves
-- | a few characters all over our HTML.
css :: forall r i. String -> HH.IProp (class :: String | r) i
css = HP.class_ <<< HH.ClassName

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    -- Introducing App monad so that we can load the `@emurgo/cardano-serialization-lib-browser`
    -- library and use it through the Purescript `foreign` bridges created in `Data.Cardano` modules.
    cardanoWasm <- loadCardanoWasm
    _ <- runUI (H.hoist (runAppM cardanoWasm) (component cardanoWasm)) unit body
    pure unit

data RemoteData e a = NotAsked
                    | Loading
                    | Failure e
                    | Success a

type State
  = { cardanoWasm :: CardanoWasm
    , tokenName :: String
    , cidM :: Maybe String
    , paymentRD :: RemoteData Error String
    , mintingRD :: RemoteData Error String
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
  , paymentRD: NotAsked
  , mintingRD: NotAsked
  }

spinner :: forall w i. HH.HTML w i
spinner =
  HH.div
    [ css "lds-roller"
    ]
    (fromFoldable $ replicate 8 (HH.div_ []))

coinLoader :: forall w i. HH.HTML w i
coinLoader =
  HH.img
    [ css "coin-loader"
    , HP.src "/assets/images/flying-money.gif"
    ]

doneImage :: forall w i. HH.HTML w i
doneImage =
  HH.img
    [ css "done-image"
    , HP.src "/assets/images/check-mark.webp"
    ]

githubLink :: forall w i. HH.HTML w i
githubLink =
  HH.a
    [ HP.href "https://github.com/carleryd/plutus-english-auction"
    , HP.target "_blank" ]
    [ HH.img
      [ css "github-link"
      , HP.src "/assets/images/github-mark.png"
      ]
    ]

statusBox :: forall w i. RemoteData Error String -> HH.HTML w i -> HH.HTML w i
statusBox rd elem =
  HH.div
    [ css "status-box" ]
    [ HH.div
      [ css "loading-container" ]
      [ case rd of
           NotAsked -> HH.text ""
           Loading -> coinLoader
           Failure _ -> HH.text ""
           Success _ -> doneImage
        ]
    , HH.span
      [ css "loading-description" ]
      [ HH.p_ [ elem ] ]
    ]

statusContainer :: forall w i. RemoteData Error String -> RemoteData Error String -> HH.HTML w i
statusContainer paymentRD mintingRD =
  let
    paymentElem = case paymentRD of
      NotAsked -> HH.text ""
      Loading -> HH.text "Sending payment..."
      Failure _ -> HH.text "Error paying app to mint NFT"
      Success _wid -> HH.text "Wallet has received payment!"
    mintingElem = case mintingRD of
      NotAsked -> HH.text ""
      Loading -> HH.text "NFT is being minted..."
      Failure _ -> HH.text "Error minting token"
      Success txh -> HH.div [ css "mint-success-container" ]
                     [ HH.p_ [ HH.text "NFT has been minted! " ]
                     , HH.p_
                       [ HH.text "Check your wallet or "
                       , HH.a
                        [ HP.href $ "https://explorer.cardano-testnet.iohkdev.io/en/transaction?id=" <> txh
                        , HP.target "_blank"
                        , css "mint-success-link"
                        ]
                        [ HH.text "view in explorer"
                        ]
                      ]
                    ]
  in
     HH.div
     [ css "status-container" ]
     [ statusBox paymentRD paymentElem
     , statusBox mintingRD mintingElem
     ]


render :: forall m. State -> H.ComponentHTML Action () m
render state = do
  HH.div
    [ css "container" ]
    [ githubLink
    , HH.img
      [ css "coin"
      , HP.src "/assets/images/coin.gif"
      ]
    , HH.div
      [ css "container-inner" ]
      [ HH.input
        [ css "input-field"
        , HP.value state.tokenName
        , HE.onValueInput SetTokenName
        ]
      , HH.button
          [ css "button"
          , HE.onClick \_ -> MintToken state.tokenName ]
          [ HH.text "MINT" ]
      ]
    , statusContainer state.paymentRD state.mintingRD
    ]

type WalletBalances
  = { w1 :: A.Json, w2 :: A.Json }

jsonToWb :: A.Json -> Either JsonDecodeError WalletBalances
jsonToWb = decodeJson

enableNami :: forall m. MonadAff m => m Unit
enableNami =
  if Nami.namiWalletInstalled then do
    _ <- H.liftAff Nami.enable
    pure unit
  else do
    liftEffect $ window >>= (alert "Please install Nami Wallet Chrome extension for this app to work.")

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
    _ <- H.liftAff enableNami
    log
      ( "Found cid: " <> show cid)

    H.modify_ (_ { cidM = Just cid })

  MintToken tokenName -> do
    if (length tokenName <= 0) then do
      eff <- liftEffect $ window >>= (alert "Token string is empty!")
      pure eff
    else do
      H.modify_ (_ { paymentRD = NotAsked, mintingRD = NotAsked } )
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

          H.modify_ (_ { paymentRD = Loading } )

          partialCborTx <- H.liftAff $ fetchContractPartialTx cid
          makePaymentRes <-
            H.lift
              $ try
              $ (balanceSignAndSubmitTx partialCborTx)

          case makePaymentRes of
            Left e -> do
              H.modify_ (_ { paymentRD = Failure e } )
              log $ show e
            Right txId -> do
              resE <- H.liftAff $ postPendingTx (show txId) tokenName (show wa)
              case resE of
                Left e -> do
                  log ("ERROR: " <> show e)
                  H.modify_ (_ { paymentRD = Failure e } )
                Right walletId -> do
                  H.modify_ (_ { paymentRD = Success walletId } )
                  H.modify_ (_ { mintingRD = Loading } )
                  mintTxE <- H.liftAff $ getFindTx tokenName walletId
                  case mintTxE of
                    Left e -> H.modify_ (_ { mintingRD = Failure e } )
                    Right tx -> H.modify_ (_ { mintingRD = Success tx } )

        Nothing -> do
          log ( "Something went wrong, either with wallet addresses or fetching contract instance" )

  SetTokenName tn -> do
    H.modify_ (_ { tokenName = toUpper tn })

newtype FindTxData =
  FindTxData
  { tokenName :: String
  , walletId :: String
  }

getFindTx :: String -> String -> Aff (Either Error String)
getFindTx tn wid = do
  -- TODO: Implement proper url encoding
  resE <- AX.get
            ResponseFormat.json
            ("/find-tx" <> "?walletId=" <> wid <> "&tokenName=" <> tn)

  case resE of
    Left e -> (pure <<< Left <<< error <<< AX.printError) e
    Right res -> case A.toString res.body of
      Just body -> pure $ Right body
      Nothing -> pure $ Left $ error "postPendingTx parsing error"

newtype PendingTxData =
  PendingTxData { txHash :: String }

postPendingTx :: String -> String -> String -> Aff (Either Error String)
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
    Right res -> case A.toString res.body of
      Just body -> pure $ Right body
      Nothing -> pure $ Left $ error "postPendingTx parsing error"

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
                { url = ("/pab/api/contract/instances/wallet/" <> wid)
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
                { url = ("/pab/api/contract/instance/" <> cid <> "/status")
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
