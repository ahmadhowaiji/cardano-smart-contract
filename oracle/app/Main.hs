import Cardano.Api (Error (displayError), writeFileTextEnvelope)
import Data.Hex (Hex (hex))
import Data.String (IsString (..))
import Ledger (TokenName, TxId (TxId), TxOutRef (TxOutRef))
import Ledger.Bytes (getLedgerBytes)
import Plutus.V1.Ledger.Value (TokenName (TokenName))
import PlutusNft (apiNFTMintScript)
import System.Environment (getArgs)
import Prelude

main :: IO ()
main = putStrLn "Hello, Haskell!"
