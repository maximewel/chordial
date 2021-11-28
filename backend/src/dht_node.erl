-module(dht_node).

-export([main/1]).
%% This process is responsible of the indexing (DHT node)

main(Store) -> 
    io:format("DHT node powering up !~n"),
    init_node(Store).

init_node(Store) ->
    NodeID = get_node_id(),
    io:format("NodeID: ~p, from : ~p~n", [NodeID, self()]),
    NodeFingers = get_fingers(),
    loop(Store, NodeID, NodeFingers).

loop(Store, NodeId, Fingers) ->
    receive
        %% From HTTP handler - first call to lookup with just a value to hash
        {store, Source, Value} ->
            store(Store, NodeId, Source, Value),
            loop(Store, NodeId, Fingers);
        %% Subsequent calls - we have key and value to avoid repeting the hash function call
        {store, Source, Key, Value} ->
            store(Store, NodeId, Source, {Key, Value}),
            loop(Store, NodeId, Fingers);
        {lookup, Source, Key} ->
            lookup(Store, Source, Key),
            loop(Store, NodeId, Fingers);
        {delete, Source, Key} ->
            delete(Store, Source, Key),
            loop(Store, NodeId, Fingers)
    end,
    loop(Store, NodeId, Fingers).

%% DHT Node initilization
%TODO init node ID ?
get_node_id() ->
    PidAsList = pid_to_list(self()),
    get_hashed(PidAsList).

get_hashed(Data) -> 
    BinData = list_to_binary(Data),
    crypto:hash(sha, BinData).

%TODO init node fingers ?
get_fingers() ->
    [].

%% DHT indexing methods
store(Store, NodeID, Source, Value) -> 
    HashKey = get_hashed([Value]),
    io:format("Key hash: ~p~n", [HashKey]),
    store(Store, NodeID, Source, HashKey, Value).

store(Store, NodeID, Source, Key, Value) -> 

    Store ! {store, self(), {Key, Value}},

    receive
        %Simply give the message back from the source, wether it is the http handler or another DHT node
        {store, success} -> Source ! {store, success}
    end.

lookup(Store, Source, Key) ->
    NodeKey = get_hashed(Key),
    io:format("Key hash: ~p~n", [NodeKey]),

    Store ! {lookup, self(), NodeKey},
    receive
        %Simply give the message back from the source, wether it is the http handler or another DHT node
        {lookup, Finds} -> Source ! {lookup, Finds}
    end.

delete(Store, Source, Key) ->
    NodeKey = get_hashed(Key),
    io:format("Key hash: ~p~n", [NodeKey]),

    Store ! {delete, self(), NodeKey},
    receive
        %Simply give the message back from the source, wether it is the http handler or another DHT node
        {delete, success} -> Source ! {delete, success}
    end.