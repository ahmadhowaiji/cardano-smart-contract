#!/bin/bash
cardano-cli transaction build --alonzo-era --testnet-magic 1097911063 --tx-in f13abf0c6e897355605032ef2657ad5bdbdb
804df8670008ece4c3cf10e7bf1c#0 --tx-in-collateral f13abf0c6e897355605032ef2657ad5bdbdb804df8670008ece4c3cf10e7bf1c#0 --tx-out addr_test1qzj0vaeaqh5atz6w9g7jfy
3ye8zdhec8aq78tkatz6capmqcpr7cnfvx2ffr7cvtaqhkqc0jrgd7vzlpg8u2349xrzlsfak99g+10000000+"1 047bb40c0aa2aedd164682f88a5e0cb03508f20004ddd4d28596d266.BlockMark" -
-change-address addr_test1qzj0vaeaqh5atz6w9g7jfy3ye8zdhec8aq78tkatz6capmqcpr7cnfvx2ffr7cvtaqhkqc0jrgd7vzlpg8u2349xrzlsfak99g --mint-redeemer-value 1 --mint "1
 047bb40c0aa2aedd164682f88a5e0cb03508f20004ddd4d28596d266.BlockMark" --mint-script-file minting-policy.plutus --protocol-params-file pparams.json --out-file t
x.build 