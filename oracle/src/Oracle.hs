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

module Oracle (apiOracleScript, Oracle, oracleData) where

import Cardano.Api (FromJSON, PlutusScript, PlutusScriptV1, ToJSON)
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised))
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SBS
import Ledger
  ( AssetClass,
    CurrencySymbol,
    Datum (Datum),
    DatumHash,
    PubKeyHash,
    ScriptContext (scriptContextTxInfo),
    TxInInfo (txInInfoResolved),
    TxInfo,
    TxOut (txOutDatumHash, txOutValue),
    Validator,
    findDatum,
    findOwnInput,
    getContinuingOutputs,
    txSignedBy,
  )
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Value as Value
  ( AssetClass (AssetClass),
    TokenName (TokenName),
    assetClassValueOf,
    geq,
  )
import qualified Plutus.V1.Ledger.Ada as Ada
import PlutusPrelude (Generic)
import qualified PlutusTx
import PlutusTx.Prelude
  ( Bool,
    Eq (..),
    Integer,
    Maybe (..),
    emptyByteString,
    isJust,
    traceError,
    traceIfFalse,
    ($),
    (&&),
    (.),
  )
import PlutusTx.Semigroup (Semigroup ((<>)))
import Prelude (Show)

data Oracle = Oracle
  { oSymbol :: !CurrencySymbol,
    oOperator :: !PubKeyHash,
    oFee :: !Integer
  }
  deriving (Show, Generic, FromJSON, ToJSON)

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

{-# INLINEABLE oracleAsset #-}
oracleAsset :: Oracle -> AssetClass
oracleAsset oracle = AssetClass (oSymbol oracle, TokenName emptyByteString)

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

oracleScriptAsCbor :: Oracle -> LB.ByteString
oracleScriptAsCbor = serialise . oracleValidator

apiOracleScript :: Oracle -> PlutusScript PlutusScriptV1
apiOracleScript = PlutusScriptSerialised . SBS.toShort . LB.toStrict . oracleScriptAsCbor

oracleData :: CurrencySymbol -> PubKeyHash -> Integer -> Oracle
oracleData = Oracle