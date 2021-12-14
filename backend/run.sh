#!/bin/bash

S_NAME="Gnogni"
PEER_IP="6f411c4c84994e4065ddcd969d6b73af0fa0b4bf"
PEER_NAME="papanoel@DESKTOP-RPCQHC6"

cd ./_build/default/lib/backend/ebin/ 
erl -noshell -sname $0 -setcookie chordial -run dht_node main $PEER_IP $PEER_NAME -s init stop