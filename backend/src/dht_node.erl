-module(dht_node).

-export([main/0, main/1, main/2]).
%% This process is responsible of the indexing (DHT node)
    
main() -> 
    {Store, Identity} = init_node(),
    Fingers = [],
    loop(Store, Identity, Fingers).

main(PeerNodeID, PeerNodeName) -> 
    {Store, Identity} = init_node(),
    Fingers = join_dht({PeerNodeID, PeerNodeName}, Identity),
    loop(Store, Identity, Fingers).

main([PeerNodeID, PeerNodeName]) -> 
    io:format("My args: ~p, ~p~n", [PeerNodeID, PeerNodeName]),
    main(PeerNodeID, PeerNodeName).

init_node()->
    io:format("DHT node powering up !~n"),
    Store = spawn(storing_interface, main, []),
    NodeID = get_node_id(),
    io:format("Identity: <~p:~p> at ~p~n", [NodeID, node(), self()]),
    register(NodeID, self()),
    Identity = {NodeID, node()},
    {Store, Identity}.

join_dht({PeerNodeID, PeerNodeName}, Identity) ->
    io:format("Peer: <~p:~p>~n", [PeerNodeID, PeerNodeName]),
    %Need ID and Name as atoms
    IdAtom = list_to_atom(PeerNodeID),
    NodeNameAtom = list_to_atom(PeerNodeName),
    NodeFingers = get_fingers({IdAtom, NodeNameAtom}, Identity),
    io:format("My fingers: ~p~n", [NodeFingers]),
    NodeFingers.

%%% Helpers %%%
get_hashed(Data) -> 
    BinData = list_to_binary(Data),
    Hash = crypto:hash(sha, BinData),
    io_lib:format("~64.16.0b", [binary:decode_unsigned(Hash)]).

%%% DHT Node initilization %%%
get_node_id() ->
    PidAsList = pid_to_list(self()),
    list_to_atom(get_hashed(PidAsList)).

get_fingers(Peer, Identity) ->
    io:format("Sending to: ~p~n", [Peer]),
    Peer ! {dht_discovery, self()},
    receive
        {dht_discovery_finished, PeerList} ->
            Fingers = fingers_in_peerlist(PeerList, Identity)
    end,
    Fingers.

%%Init, start extracting last one to make a nice circlular array
fingers_in_peerlist(PeerList, Identity) ->
    {Min, Max} = min_max_fingers(PeerList),
    Predecessor = lists:last(PeerList),
    fingers_in_peerlist(PeerList, Predecessor, Identity, Min, Max).
%% We have made the whole circle without any peer < us, we are the new little one
fingers_in_peerlist([], _, Identity, Min, Max) ->
    Min ! { predecessor, Identity },
    Max ! { successor, Identity },
    [{pred, Max}, {succ, Min}];
%%Right condition - we are between next and last
fingers_in_peerlist([{NextPeerID, NextPeerNode} | PeerList], {LastPeerID, LastPeerNode}, {NodeID, NodeName}, _, _) when NextPeerID > NodeID, LastPeerID < NodeID ->
    LastPeer = {LastPeerID, LastPeerNode},
    NextPeer = {NextPeerID, NextPeerNode},
    Identity = {NodeID, NodeName},
    NextPeer ! { predecessor, Identity },
    LastPeer ! { successor, Identity },
    [{pred, LastPeer}, {succ, NextPeer}];
%%Keep exploring
fingers_in_peerlist([NextPeer | PeerList], LastPeer, NodeID, Min, Max) -> 
    fingers_in_peerlist(PeerList, LastPeer, NodeID, Min, Max).

min_max_fingers([First | PeerList]) ->
    min_max_fingers(PeerList, First, First).

min_max_fingers([], Min, Max) ->
    {Min, Max};
min_max_fingers([{CurrentID, CurrentNodeName} | PeerList], {MinID, MinNodeName}, {MaxID, MaxNodeName}) when CurrentID < MinID ->
    min_max_fingers(PeerList, {CurrentID, CurrentNodeName}, {MaxID, MaxNodeName});
min_max_fingers([{CurrentID, CurrentNodeName} | PeerList], {MinID, MinNodeName}, {MaxID, MaxNodeName}) when CurrentID > MaxID ->
    min_max_fingers(PeerList, {MinID, MinNodeName}, {CurrentID, CurrentNodeName});
min_max_fingers([{CurrentID, CurrentNodeName} | PeerList], {MinID, MinNodeName}, {MaxID, MaxNodeName}) ->
    min_max_fingers(PeerList, {MinID, MinNodeName}, {MaxID, MaxNodeName}).
    

loop(Store, Identity, Fingers) ->

    receive
        %%From HTTP handler
        {store, Source, Key, Value} ->
            store(Store, Identity, Fingers, Source, Key, Value);
        %%From another node, already hashed to avoid re-hashing constantly
        {store, hashed, Source, Key, Value} ->
            store(hashed, Store, Identity, Fingers, Source, Key, Value);
        {lookup, Source, Key} ->
            lookup(Store, Source, Key);
        {lookup, hashed, Source, Key} ->
            lookup(hashed, Store, Source, Key);

        %% Messages when a DHT node wants to know every node in the DHT node
        {dht_discovery, Source} ->
            discovery_step(Source, Identity, Fingers);
        {dht_discovery, Source, PeerList} ->
            discovery_step(Source, Identity, PeerList, Fingers);

        %%Update messages to change pred/succ
        {predecessor, NewPred} -> 
            io:format("New pred: ~p~n", [NewPred]),
            NewFingers = replace_finger(Fingers, {pred, NewPred}),
            loop(Store, Identity, NewFingers);
        {successor, NewSucc} -> 
            io:format("New successor: ~p~n", [NewSucc]),
            NewFingers = replace_finger(Fingers, {succ, NewSucc}),
            loop(Store, Identity, NewFingers);

        %%GUI asked for the state of the dht
        {state, Source} ->
            state_step(Identity, Source, Fingers, Store);
        {state, Source, Init, States} ->
            state_step(Identity, Source, Fingers, Store, Init, States)
        
        end,
    loop(Store, Identity, Fingers).

replace_finger(Fingers, {Key, Finger}) ->
    lists:keytake(Key, 1, Fingers),
    Fingers ++ [{Key, Finger}].

%% DHT discovery
discovery_step(Source, Identity, []) ->
    Source ! {dht_discovery_finished, [Identity]};
discovery_step(Source, Identity, Fingers) ->
    io:format("Call for DHT discovery~n"),
    case lists:keyfind(succ, 1, Fingers) of
        false -> Source ! {dht_discovery_finished, [Identity]};
        {succ, Successor} -> Successor ! {dht_discovery, Source, [Identity]}
    end.

discovery_step(Source, Identity, [Init | PeerList], Fingers) when Init == Identity ->
    io:format("DHT discovery finished, peers: ~p~n", [[Init] ++ PeerList]),
    Source ! {dht_discovery_finished, [Init] ++ PeerList};
discovery_step(Source, Identity, PeerList, Fingers) ->
    io:format("DHT discovery step: ~n"),
    {succ, Successor} = lists:keyfind(succ, 1, Fingers),
    Successor ! {dht_discovery, Source, PeerList ++ [Identity]}.

%%% STORE
store(Store, Identity, Fingers, Source, Key, Value) -> 
    HashKey = get_hashed([Key]),
    io:format("Key hash: ~p~n", [HashKey]),
    store(hashed, Store, Identity, Fingers, Source, HashKey, Value).

store(hashed, Store, [NodeID, _], [{pred, PredId}, {succ, SuccId}], Source, Key, Value) ->
    if 
        NodeID >= Key -> 
            if
                Key > PredId , PredId >= NodeID -> 
                    store(Store, Source, Key, Value);
                true -> 
                    PredId ! {store, hashed, Source, Key, Value}
            end;
        
        true -> 
            if 
                PredId >= NodeID ; Key > PredId ->
                    store(Store, Source, Key, Value);
                true -> 
                    SuccId ! {store, hashed, Source, Key, Value}
            end
    end.

store(Store, Source, Key, Value) -> 
    Store ! {store, self(), {Key, Value}},

    receive 
        %Simply give the message back from the source, wether it is the http handler or another DHT node
        {store, success} -> Source ! {store, success}
    end.

%% LOOKUP
lookup(Store, Source, Key) ->
    NodeKey = get_hashed(Key),
    io:format("Key hash: ~p~n", [NodeKey]),
    lookup(hashed, Store, Source, NodeKey).

lookup(hashed, Store, Source, Key) ->
    Store ! {lookup, self(), Key},

    receive
        %Simply give the message back from the source, wether it is the http handler or another DHT node
        {lookup, Finds} -> Source ! {lookup, Finds}
    end.

%% STATE OF THE DHT (data dump)
%Init
state_step(Identity, Source, Fingers, Store) ->
    io:format("Initialisation - DHT state~n"),
    Datadump = self_data_dump(Identity, Fingers, Store),
    {succ, Successor} = lists:keyfind(succ, 1, Fingers),
    Successor ! {state, Source, Identity, [Datadump]}.

%Case: We have made a whole round
state_step(Identity, Source, Fingers, Store, Init, States) when Init == Identity->
    io:format("GHT state over, sending back to friend at ~p~n", [Source]),
    Source ! {state_finished, States};
    
%Regular step
state_step(Identity, Source, Fingers, Store, Init, States) ->
    io:format("DHT step, me: ~p, init: ~p~n", [Identity, Init]),
    Datadump = self_data_dump(Identity, Fingers, Store),
    {succ, Successor} = lists:keyfind(succ, 1, Fingers),
    Successor ! {state, Source, Init, States ++ [Datadump]}.

%%% Data-dump this node 
self_data_dump(Identity, Fingers, Store) ->
    Store ! {data_dump, Identity},
    receive
        {data_dump_result, Storage} -> true
    end,
    {{identity, Identity}, {fingers, Fingers}, {storage, Storage}}.