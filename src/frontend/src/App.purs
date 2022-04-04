module App where

import Prelude
import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson)
import Data.Either (Either(..), fromRight)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument (toDocument)
import Cardano.Wallet.Nami as Nami

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body

type State
  = Maybe WalletBalances

data Action
  = Initialize
  | FetchBalances
  | GetNamiBalance

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just Initialize
              }
    }

initialState :: forall input. input -> State
initialState _ = Nothing

render :: forall m. State -> H.ComponentHTML Action () m
render state = do
  HH.div_
    [ HH.h1_
        [ HH.text "Balances" ]
    , fromMaybe
        (HH.text "No wallets found")
        ( ( \wb ->
              HH.div_
                [ HH.h2_ [ HH.text "Wallet 1" ]
                , (renderWallet wb.w1)
                , HH.h2_ [ HH.text "Wallet 2" ]
                , (renderWallet wb.w2)
                ]
          )
            <$> state
        )
    , HH.button
        [ HE.onClick \_ -> FetchBalances ]
        [ HH.text "Refresh balances" ]
    , HH.button
        [ HE.onClick \_ -> GetNamiBalance ]
        [ HH.text "Nami" ]
    ]

type WalletBalances
  = { w1 :: Json, w2 :: Json }

jsonToWb :: Json -> Either JsonDecodeError WalletBalances
jsonToWb = decodeJson

-- TODO: Json for now, should be properly decoded datatype in future
renderWallet :: forall w i. Json -> HH.HTML w i
renderWallet wallet = do
  HH.p_ [ HH.text $ stringify wallet ]

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    -- handleAction Regenerate
    handleAction FetchBalances
    balances <- H.get
    log
      ( "Balances: "
          <> maybe "None"
              (\wb -> (stringify wb.w1) <> (stringify wb.w2))
              balances
      )
  FetchBalances -> do
    result <-
      H.liftAff
        $ AX.request
            ( AX.defaultRequest
                { url = "/balances"
                , method = Left GET
                , responseFormat = ResponseFormat.json
                }
            )
    case result of
      Left err -> log $ "GET /balances response failed to decode: " <> AX.printError err
      Right response -> do
        H.put $ fromRight Nothing (Just <$> jsonToWb response.body)
  GetNamiBalance -> do
    -- w <- H.liftEffect $ document <$> window
    -- let x = 5
    log ("HELLO" <> show Nami.isEnabled)
