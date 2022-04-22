module Data.Cardano.Address
  ( Address
  , fromBech32
  , toBech32
  , fromBytes
  ) where

import Prologue

import Control.Monad.Reader (class MonadAsk, asks)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Cardano (CardanoWasm)
import Effect.Aff (Error)

foreign import data Address :: Type

foreign import fromBech32Impl :: forall r. CardanoWasm -> String -> (Error -> r) -> (Address -> r) -> r

fromBech32 :: forall m. MonadAsk CardanoWasm m => String -> m (Either Error Address)
fromBech32 bech32 = asks \wasm -> fromBech32Impl wasm bech32 Left Right

foreign import toBech32 :: Address -> String

foreign import fromBytesImpl :: forall r. CardanoWasm -> Uint8Array -> (Error -> r) -> (Address -> r) -> r

fromBytes :: forall m. MonadAsk CardanoWasm m => Uint8Array -> m (Either Error Address)
fromBytes bytes = asks \wasm -> fromBytesImpl wasm bytes Left Right
