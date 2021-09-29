import Cardano.Api (Error (displayError), writeFileTextEnvelope)
import Data.String (IsString (..))
import Ledger (PubKeyHash (PubKeyHash))
import Ledger.Bytes (getLedgerBytes)
import Oracle (apiOracleScript, oracleData)
import Plutus.V1.Ledger.Value (CurrencySymbol (CurrencySymbol))
import System.Environment (getArgs)
import Prelude

main :: IO ()
main = do
  args <- getArgs
  print $ head args
  let oracleCurrencySymbol = parseCurrencySymbol $ head args
      oracleOperator = parsePubKeyHash $ args !! 1
      oracleFee = parseFee $ args !! 2
      oracleScriptFile = "plutus-script/oracle.plutus"
      oracle = oracleData oracleCurrencySymbol oracleOperator oracleFee

  oraclePolicyResult <- writeFileTextEnvelope oracleScriptFile Nothing $ apiOracleScript oracle
  case oraclePolicyResult of
    Left err -> print $ displayError err
    Right () -> putStrLn $ "wrote Oracle Script to file " ++ oracleScriptFile

parseCurrencySymbol :: String -> CurrencySymbol
parseCurrencySymbol = CurrencySymbol . getLedgerBytes . fromString

parsePubKeyHash :: String -> PubKeyHash
parsePubKeyHash = PubKeyHash . getLedgerBytes . fromString

parseFee :: String -> Integer
parseFee s = read s :: Integer
