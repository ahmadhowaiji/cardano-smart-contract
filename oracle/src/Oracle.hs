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
{-# OPTIONS_GHC -Wno-unused-imports #-}

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
    ScriptContext (scriptContextTxInfo),
    TxInInfo (txInInfoResolved),
    TxInfo (txInfoSignatories),
    TxOut (txOutDatumHash, txOutValue),
    Validator,
    findDatum,
    findOwnInput,
    getContinuingOutputs,
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
import PlutusTx.Builtins.Class (stringToBuiltinString)
import PlutusTx.Prelude
  ( Bool (False, True),
    Eq (..),
    Integer,
    Maybe (..),
    emptyByteString,
    isJust,
    traceError,
    traceIfFalse,
    (&&),
    (.),
  )
import PlutusTx.Semigroup (Semigroup ((<>)))
import Prelude (Show)

data Oracle = Oracle
  { oSymbol :: !CurrencySymbol,
    oFee :: !Integer,
    oUpdateCode :: !Integer
  }
  deriving (Show, Generic, FromJSON, ToJSON)

PlutusTx.makeLift ''Oracle

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
mkOracleValidator :: Oracle -> Integer -> Integer -> ScriptContext -> Bool
mkOracleValidator oracle x r ctx = do
  let updateValueInt = oUpdateCode oracle
  traceIfFalse "token missing from input" inputHasToken
    && traceIfFalse "token missing from output" outputHasToken
    && if r == updateValueInt
      then traceIfFalse "invalid output datum" validOutputDatum
      else
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
    validOutputDatum = case outputDatum of
      Nothing -> False
      Just _ -> True

    feesPaid :: Bool
    feesPaid =
      let inVal = txOutValue ownInput
          outVal = txOutValue ownOutput
       in outVal `geq` (inVal <> Ada.lovelaceValueOf (oFee oracle))

data Oracling

instance Scripts.ValidatorTypes Oracling where
  type DatumType Oracling = Integer
  type RedeemerType Oracling = Integer

typedOracleValidator :: Oracle -> Scripts.TypedValidator Oracling
typedOracleValidator oracle =
  Scripts.mkTypedValidator @Oracling
    ($$(PlutusTx.compile [||mkOracleValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode oracle)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @Integer @Integer

oracleValidator :: Oracle -> Validator
oracleValidator = Scripts.validatorScript . typedOracleValidator

oracleScriptAsCbor :: Oracle -> LB.ByteString
oracleScriptAsCbor = serialise . oracleValidator

apiOracleScript :: Oracle -> PlutusScript PlutusScriptV1
apiOracleScript = PlutusScriptSerialised . SBS.toShort . LB.toStrict . oracleScriptAsCbor

oracleData :: CurrencySymbol -> Integer -> Integer -> Oracle
oracleData = Oracle