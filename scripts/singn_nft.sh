#!/bin/bash
cardano-cli transaction sign \ 
--tx-body-file tx.build \
--signing-key-file $1 \
--testnet-magic 1097911063 \
--out-file tx.signed

