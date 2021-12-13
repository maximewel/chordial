#!/bin/bash

S_NAME="mamanoel"
PEER_IP="0000000000000000000000006f411c4c84994e4065ddcd969d6b73af0fa0b4bf"
PEER_NAME="papanoel@DESKTOP-RPCQHC6"

cd ./_build/default/lib/backend/ebin/ 
erl -noshell -sname $S_NAME -setcookie chordial -run dht_node main $PEER_IP $PEER_NAME -s init stop