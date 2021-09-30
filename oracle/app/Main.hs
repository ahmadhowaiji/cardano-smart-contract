import Cardano.Api (Error (displayError), writeFileTextEnvelope)
import Data.String (IsString (..))
import Oracle (apiOracleScript, oracleData)
import Plutus.V1.Ledger.Api
  ( LedgerBytes (getLedgerBytes),
  )
import Plutus.V1.Ledger.Value (CurrencySymbol (CurrencySymbol))
import System.Environment (getArgs)
import Prelude

main :: IO ()
main = do
  args <- getArgs
  print $ head args
  let oracleCurrencySymbol = parseCurrencySymbol $ head args
      oracleFee = parseInt $ args !! 1
      updateValue = parseInt $ args !! 1
      oracleScriptFile = "plutus-script/oracle.plutus"
      oracle = oracleData oracleCurrencySymbol oracleFee updateValue

  oraclePolicyResult <- writeFileTextEnvelope oracleScriptFile Nothing $ apiOracleScript oracle
  case oraclePolicyResult of
    Left err -> print $ displayError err
    Right () -> putStrLn $ "wrote Oracle Script to file " ++ oracleScriptFile

parseCurrencySymbol :: String -> CurrencySymbol
parseCurrencySymbol = CurrencySymbol . getLedgerBytes . fromString

parseInt :: String -> Integer
parseInt s = read s :: Integer
