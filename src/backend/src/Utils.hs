{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils where

import Blockfrost.API ()
import Blockfrost.Client
  ( Address (Address),
    Amount,
    Block,
    BlockHash,
    BlockfrostError (..),
    Transaction,
    getAddressDetails,
    getAddressUtxos,
    getBlock,
    getLatestBlock,
    getTx,
    projectFromFile,
    runBlockfrost,
  )
import Blockfrost.Lens hiding (port)
import Blockfrost.Types.Cardano.Addresses (AddressUtxo)
import Blockfrost.Types.Cardano.Blocks
import Blockfrost.Types.Cardano.Transactions
import Blockfrost.Types.Shared
import Contract.PAB (TokenContracts (..))
import Contract.Utils (contractActivationArgs, unsafeReadAddress, unsafeReadWalletId)
import Control.Exception (throwIO)
import qualified Control.Lens as Lens
import Control.Monad (join, when)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Bifunctor (first)
import Data.Text (pack, unpack)
import qualified Data.Text
import GHC.Generics
import Network.HTTP.Req
import Plutus.PAB.Events.ContractInstanceState (PartiallyDecodedResponse (..))
import Plutus.PAB.Webserver.Types (ContractInstanceClientState (..))
import qualified System.Directory
import Text.Printf (printf)

w1Address :: Address
w1Address =
  Address $
    Data.Text.pack
      "addr_test1qplfk3cw70jwlq7gya2m0mrx6fxujrf6gm9v9c556j75v5k5qqhj89csyv7y7543l24hz8t00rrgyct2p5gv3udgtpxqy637fp"

w2Address :: Address
w2Address =
  Address $
    Data.Text.pack
      "addr_test1qp2ay4yk83x2hyv63y794klajtj08w2kawk2vyc4adpu5urd8fkz57x5aymr2xvfqqg4drzw384cx0jnqmt6d4vdwfeq934a74"

printAddressUtxos :: [AddressUtxo] -> [IO ()]
printAddressUtxos utxos =
  printedAmounts
  where
    xss :: [[Amount]]
    xss = (Lens.^. amount) <$> utxos

    printAmount :: Amount -> IO ()
    printAmount x = print $ "Amount: " ++ show x

    printedAmounts = (\amounts -> print "Utxo: " <> mconcat (printAmount <$> amounts)) <$> xss

data WalletBalances = WalletBalances
  { w1 :: [AddressUtxo],
    w2 :: [AddressUtxo]
  }
  deriving (Show, Generic)

instance ToJSON WalletBalances

instance FromJSON WalletBalances

cardanoServicesIP :: Data.Text.Text
cardanoServicesIP = "localhost"

-- cardanoServicesIP = "192.168.1.103"

getWalletInfo :: String -> IO String
getWalletInfo _wid = do
  let wid = unsafeReadWalletId _wid
  v <-
    runReq defaultHttpConfig $
      req
        GET
        ( http cardanoServicesIP
            /: "v2"
            /: "wallets"
            /: pack _wid
        )
        NoReqBody
        jsonResponse
        (port 8090)

  let c = responseStatusCode v
  when (c /= 200) $
    throwIO $
      userError $
        printf
          "ERROR: %d\n"
          c

  let x :: ContractInstanceClientState TokenContracts
      x = responseBody v

  return "HELLO"

getBlockConfirmations :: TxHash -> IO (Either BlockfrostError Integer)
getBlockConfirmations txId = do
  currentDir <- System.Directory.getCurrentDirectory
  testnet <- projectFromFile (currentDir <> "/.blockfrost-testnet-token")

  transactionRes <-
    runBlockfrost testnet $
      getTx txId

  let blockHashE = _transactionBlock <$> transactionRes

  blockE <- runBlockfrost testnet (getBlock $ first (const 0) blockHashE)

  let confirmationsE = _blockConfirmations <$> blockE

  return confirmationsE

getBlockfrostUtxos :: IO WalletBalances
getBlockfrostUtxos = do
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
