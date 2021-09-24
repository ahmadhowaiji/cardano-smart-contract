import Cardano.Api ( writeFileTextEnvelope, Error(displayError) )
import Data.String                         (IsString (..))
import Data.Hex ( Hex(hex) ) 
import Ledger ( TxOutRef(TxOutRef), TxId(TxId), TokenName )
import Ledger.Bytes                        (getLedgerBytes)
import Prelude
import System.Environment                  (getArgs)

import PlutusNft ( apiNFTMintScript )
import Plutus.V1.Ledger.Value (TokenName(TokenName))

main :: IO ()
main = do
  args <- getArgs
  print $ head args
  let name            = parsTokenName $  head args
      utxo            = parseUTxO  $ args !! 1
      nftPolicyFile   = "plutus-script/mint-nft-plicy.plutus"

  nftPolicyResult <- writeFileTextEnvelope nftPolicyFile Nothing $ apiNFTMintScript name utxo
  case nftPolicyResult of
      Left err -> print $ displayError err
      Right () -> putStrLn $ "wrote NFT policy to file " ++ nftPolicyFile





parsTokenName :: String -> TokenName 
parsTokenName name = TokenName $ getLedgerBytes $ fromString $ hex $ fromString name 


parseUTxO :: String -> TxOutRef
parseUTxO s =
  let
    (x, y) = span (/= '#') s
  in
    TxOutRef (TxId $ getLedgerBytes $ fromString x) $ read $ tail y