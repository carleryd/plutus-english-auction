{-# LANGUAGE DeriveGeneric #-}

module Lib where

import Blockfrost.API ()
import Blockfrost.Client
  ( Address (Address),
    Amount,
    Block,
    BlockfrostError (..),
    getAddressDetails,
    getAddressUtxos,
    getLatestBlock,
    projectFromFile,
    runBlockfrost,
  )
import Blockfrost.Lens
import Blockfrost.Types.Cardano.Addresses (AddressUTXO)
import qualified Control.Lens as Lens
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Text
import GHC.Generics
import qualified System.Directory

func :: Int -> Int
func x = x * x

w1Address :: Address
w1Address =
  Address $
    Data.Text.pack
      "addr_test1vr74xzf982rzs8lmlm3qulrrkjmasw7uaccfh2qsm9nrn3qc0luyc"

w2Address :: Address
w2Address =
  Address $
    Data.Text.pack
      "addr_test1vqts6ywtgu0qv06cpxap9mhn2k6f7grktz4dn07ns86uxrgqqagks"

printAddressUtxos :: [AddressUTXO] -> [IO ()]
printAddressUtxos utxos =
  printedAmounts
  where
    xss :: [[Amount]]
    xss = (Lens.^. amount) <$> utxos

    printAmount :: Amount -> IO ()
    printAmount x = print $ "Amount: " ++ show x

    printedAmounts = (\amounts -> print "Utxo: " <> mconcat (printAmount <$> amounts)) <$> xss

data WalletBalances = WalletBalances
  { w1 :: [AddressUTXO],
    w2 :: [AddressUTXO]
  }
  deriving (Show, Generic)

instance ToJSON WalletBalances

instance FromJSON WalletBalances

getUtxos :: IO WalletBalances
getUtxos = do
  currentDir <- System.Directory.getCurrentDirectory
  testnet <- projectFromFile (currentDir <> "/.blockfrost-testnet-token")

  let c1 = getAddressUtxos w1Address
      c2 = getAddressUtxos w2Address

  w1UtxosResult <- runBlockfrost testnet c1
  w2UtxosResult <- runBlockfrost testnet c2

  _ <- case w1UtxosResult of
    Left e -> print $ "Error: " <> show e
    Right a -> print $ "Success utxo: " <> show a

  let w1Utxos = w1UtxosResult Lens.^. Lens._Right
      w2Utxos = w2UtxosResult Lens.^. Lens._Right

  return $ WalletBalances w1Utxos w2Utxos

-- printDetails :: IO [()]
-- printDetails = do
--   wb <- getUtxos
--   sequence $ printAddressUtxos (w1 wb) <> printAddressUtxos (w2 wb)
