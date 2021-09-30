#!/bin/bash
cardano-cli transaction build \
--alonzo-era \
--testnet-magic 1097911063 \
--tx-in c94fbf5510c300005d9ca17c9b6baf857a554731d69bea60952cba7733a43fb6#0 \
--tx-in-collateral c94fbf5510c300005d9ca17c9b6baf857a554731d69bea60952cba7733a43fb6#0 \
--tx-out addr_test1qzj0vaeaqh5atz6w9g7jfy3ye8zdhec8aq78tkatz6capmqcpr7cnfvx2ffr7cvtaqhkqc0jrgd7vzlpg8u2349xrzlsfak99g+10000000+"1 047bb40c0aa2aedd164682f88a5e0cb03508f20004ddd4d28596d266" \
--change-address addr_test1qzj0vaeaqh5atz6w9g7jfy3ye8zdhec8aq78tkatz6capmqcpr7cnfvx2ffr7cvtaqhkqc0jrgd7vzlpg8u2349xrzlsfak99g \
--mint-redeemer-value 1 --mint "1 047bb40c0aa2aedd164682f88a5e0cb03508f20004ddd4d28596d266" \
--mint-script-file mint-oracle-nft.plutus \
--protocol-params-file pparams.json \
--out-file tx.build
