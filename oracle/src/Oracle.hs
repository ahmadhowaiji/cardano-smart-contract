{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Oracle
  ( Oracle,
    OracleRedeemer,
    oracleValue,
    oracleTokenName,
    oracleAsset,
    mkOracleValidator,
    typedOracleValidator,
  )
where

import Cardano.Api (FromJSON, ToJSON)
import Data.Hex (Hex (hex))
import Data.String (IsString (fromString))
import Flat (Generic)
import Ledger.Tx (TxOut (txOutValue))
import qualified Ledger.Typed.Scripts as Scripts
import qualified Plutus.V1.Ledger.Ada as Ada
import Plutus.V1.Ledger.Api (CurrencySymbol, Datum (Datum), DatumHash, LedgerBytes (getLedgerBytes), PubKeyHash, ScriptContext (scriptContextTxInfo), TokenName (TokenName), TxInInfo (txInInfoResolved), Validator)
import Plutus.V1.Ledger.Contexts (TxInfo, TxOut (txOutDatumHash), findDatum, findOwnInput, getContinuingOutputs, txSignedBy)
import Plutus.V1.Ledger.Value (AssetClass (AssetClass), assetClassValueOf, geq)
import qualified PlutusTx
import PlutusTx.Prelude (Bool, Eq ((==)), Integer, Maybe (Just, Nothing), Semigroup ((<>)), isJust, traceError, traceIfFalse, ($), (&&), (.))
import Prelude (Eq, Ord, Show)

data Oracle = Oracle
  { oSymbol :: !CurrencySymbol,
    oOperator :: !PubKeyHash,
    oFee :: !Integer,
    oAsset :: !AssetClass
  }
  deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)

PlutusTx.makeLift ''Oracle

data OracleRedeemer = Update | Use
  deriving (Show)

PlutusTx.unstableMakeIsData ''OracleRedeemer

{-# INLINEABLE oracleValue #-}
oracleValue :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe Integer
oracleValue o f = do
  dh <- txOutDatumHash o
  Datum d <- f dh
  PlutusTx.fromBuiltinData d

{-# INLINEABLE oracleTokenName #-}
oracleTokenName :: TokenName
oracleTokenName = TokenName $ getLedgerBytes $ fromString $ hex $ fromString ""

{-# INLINEABLE oracleAsset #-}
oracleAsset :: Oracle -> AssetClass
oracleAsset oracle = AssetClass (oSymbol oracle, oracleTokenName)

{-# INLINEABLE mkOracleValidator #-}
mkOracleValidator :: Oracle -> Integer -> OracleRedeemer -> ScriptContext -> Bool
mkOracleValidator oracle x r ctx =
  traceIfFalse "token missing from input" inputHasToken
    && traceIfFalse "token missing from output" outputHasToken
    && case r of
      Update ->
        traceIfFalse "operator signature missing" (txSignedBy info $ oOperator oracle)
          && traceIfFalse "invalid output datum" validOutputDatum
      Use ->
        traceIfFalse "oracle value changed" (outputDatum == Just x)
          && traceIfFalse "fees not paid" feesPaid
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
      Nothing -> traceError "oracle input missing"
      Just i -> txInInfoResolved i

    inputHasToken :: Bool
    inputHasToken = assetClassValueOf (txOutValue ownInput) (oracleAsset oracle) == 1

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
      [o] -> o
      _ -> traceError "expected exactly one oracle output"

    outputHasToken :: Bool
    outputHasToken = assetClassValueOf (txOutValue ownOutput) (oracleAsset oracle) == 1

    outputDatum :: Maybe Integer
    outputDatum = oracleValue ownOutput (`findDatum` info)

    validOutputDatum :: Bool
    validOutputDatum = isJust outputDatum

    feesPaid :: Bool
    feesPaid =
      let inVal = txOutValue ownInput
          outVal = txOutValue ownOutput
       in outVal `geq` (inVal <> Ada.lovelaceValueOf (oFee oracle))

data Oracling

instance Scripts.ValidatorTypes Oracling where
  type DatumType Oracling = Integer
  type RedeemerType Oracling = OracleRedeemer

typedOracleValidator :: Oracle -> Scripts.TypedValidator Oracling
typedOracleValidator oracle =
  Scripts.mkTypedValidator @Oracling
    ($$(PlutusTx.compile [||mkOracleValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode oracle)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @Integer @OracleRedeemer

oracleValidator :: Oracle -> Validator
oracleValidator = Scripts.validatorScript . typedOracleValidator
