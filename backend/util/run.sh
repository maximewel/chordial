#!/bin/bash

S_NAME="a"
PEER_IP="566681eec0bdd6886811a854f06fc6c567a8be0a"
PEER_NAME="papanoel@DESKTOP-RPCQHC6"

cd ../_build/default/lib/backend/ebin/ 
erl -noshell -sname $S_NAME -setcookie chordial -run dht_node main $PEER_IP $PEER_NAME -s init stop