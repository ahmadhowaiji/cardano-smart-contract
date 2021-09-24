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

module PlutusNft where

import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as LB
import Ledger
  ( Script,
    ScriptContext (scriptContextTxInfo),
    TokenName,
    TxInInfo (txInInfoOutRef),
    TxInfo (txInfoInputs, txInfoMint),
    TxOutRef,
    Validator (Validator),
    mkMintingPolicyScript,
    unMintingPolicyScript,
  )
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Value as Value (flattenValue)
import qualified PlutusTx
import PlutusTx.Prelude
  ( Bool (False),
    BuiltinData,
    Eq ((==)),
    any,
    traceIfFalse,
    ($),
    (&&),
  )

{-# INLINEABLE mkNftPolicy #-}
mkNftPolicy :: TokenName -> TxOutRef -> BuiltinData -> ScriptContext -> Bool
mkNftPolicy nftName utxo _ ctx =
  traceIfFalse "Utxo not consumed" hasUtxo
    && traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUtxo :: Bool
    hasUtxo = any (\i -> txInInfoOutRef i == utxo) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue $ txInfoMint info of
      [(_, nftName', tokenAmount)] -> nftName == nftName' && tokenAmount == 1
      _ -> False

nftPolicy :: TokenName -> TxOutRef -> Scripts.MintingPolicy
nftPolicy tn utxo =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||\tn' utxo' -> Scripts.wrapMintingPolicy $ mkNftPolicy tn' utxo'||])
      `PlutusTx.applyCode` PlutusTx.liftCode tn
      `PlutusTx.applyCode` PlutusTx.liftCode utxo

nftScript :: TokenName -> TxOutRef -> Script
nftScript nftName utxo = unMintingPolicyScript $ nftPolicy nftName utxo

nftValidator :: TokenName -> TxOutRef -> Validator
nftValidator nftName utxo = Validator $ nftScript nftName utxo

nftScriptAsCbor :: TokenName -> TxOutRef -> LB.ByteString
nftScriptAsCbor nftName utxo = serialise $ nftValidator nftName utxo