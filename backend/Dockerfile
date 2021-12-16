FROM erlang:22

RUN rebar3 clean
RUN rebar3 compile

COPY /_build/default/lib/backend/ebin/ .

#Start dht node
ENTRYPOINT erl -noshell -sname $S_NAME -setcookie chordial -run dht_node main $PEER_IP $PEER_NAME -s init stop