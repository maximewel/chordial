-module(dht_http_handler).

-export([init/2]).

-define(QUOTE, "\"").

%Store
init(Req0=#{path := <<"/store">>}, State) ->
    %%Retreive kea/value
    #{key := BinaryKey} = cowboy_req:match_qs([key], Req0),
    #{value := BinaryValue} = cowboy_req:match_qs([value], Req0),
    Key = binary_to_list(BinaryKey),
    Value = binary_to_list(BinaryValue),
    io:format("Key : ~p, Value: ~p~n", [Key, Value]),

    %%Send it to store
    {_, DhtNode} = lists:keyfind(dht_node, 1, State),
    DhtNode ! {store, self(), Key, Value},

    receive
        {store, success} ->
            Req = cowboy_req:reply(200,
            #{<<"content-type">> => <<"text/plain">>},
            "You stored " ++ Key ++ "=" ++ Value,
            Req0)
    end,

    {ok, Req, State};

%Look up
init(Req0=#{path := <<"/lookup">>}, State) ->
    %%Retreive kea/value
    #{key := BinaryKey} = cowboy_req:match_qs([key], Req0),
    Key = binary_to_list(BinaryKey),
    io:format("Key to search: ~p", [Key]),

    %%Send it to store
    {_, DhtNode} = lists:keyfind(dht_node, 1, State),
    DhtNode ! {lookup, self(), Key},

    receive
        {lookup, Node, Finds} ->
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                lookup_to_json(Node, Finds),
                Req0)
    end,
    
    {ok, Req, State};

%State
init(Req0=#{path := <<"/state">>}, State) ->
    %%Retreive kea/value
    io:format("Fetching DHT state~n"),

    %%Send it to store
    {_, DhtNode} = lists:keyfind(dht_node, 1, State),
    DhtNode ! {state, self()},

    receive
        {state_finished, DHT_state} ->
            io:format("Formating DHT table ~n"),
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                state_to_json(DHT_state),
                Req0)
    end,
    
    {ok, Req, State};

%Default
init(Req0, State) ->
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        "{\"Message\" : \"Hello\"}",
        Req0),
    
    {ok, Req, State}.


%%% Helpers - JSONIFY THE DHT STATE %%%
state_to_json([FirstNode | States]) ->
    "{ " ++ in_quotes("nodes") ++ " : [" ++
        state_to_json(node_to_json(FirstNode), States) ++ 
    "]}".

state_to_json(Json, []) -> Json;
state_to_json(Json, [Node | States]) ->
    state_to_json(Json ++ "," ++ node_to_json(Node), States).

node_to_json(Node) ->
    {{identity, Identity}, {fingers, Fingers}, {storage, Storage}} = Node,
    "{" 
        ++ identity_to_json(Identity) ++ ","
        ++ fingers_to_json(Fingers) ++ ","
        ++ storage_to_json(Storage) ++
    "}".

identity_to_json({NodeID, NodeName}) ->
    in_quotes("id") ++ " : " ++ in_quotes(atom_to_list(NodeID)) ++ ","
    ++ in_quotes("node_name") ++ " : " ++ in_quotes(atom_to_list(NodeName)).

fingers_to_json(Fingers) ->
    in_quotes("fingers") ++ " : {" ++
        fing_to_json(Fingers) ++ 
    "}".

fing_to_json([]) -> "";
fing_to_json([Finger | FingerList]) -> fingers_to_json(finger_to_json(Finger), FingerList).

fingers_to_json(Json, []) -> Json;
fingers_to_json(Json, [Finger | FingerList]) ->
    fingers_to_json(Json ++ "," ++ finger_to_json(Finger), FingerList).

finger_to_json(Finger) ->
    {Key, FingerIdentity} = Finger,
    in_quotes(atom_to_list(Key)) ++ " : { " 
        ++ identity_to_json(FingerIdentity) ++
    "}".

storage_to_json(Storage) ->
    in_quotes("storage") ++ " : [" ++ store_to_json(Storage) ++ "]".

store_to_json([]) -> "";
store_to_json([First | Store]) ->
    store_to_json(store_value_to_json(First), Store).

store_to_json(Json, []) ->
    Json;
store_to_json(Json, [Value | Store]) ->
    store_to_json(Json ++ "," ++ store_value_to_json(Value), Store).

store_value_to_json({StoreKey, StoreValue}) ->
    "{" ++ in_quotes(atom_to_list(StoreKey)) ++ " : " ++ in_quotes(StoreValue) ++ "}".

lookup_to_json(Node, Finds) ->
    "{ " ++  
        in_quotes("node") ++ " : {" ++ identity_to_json(Node) ++ " }," ++
        in_quotes("values") ++ " : " ++ values_to_json(Finds) ++
    "}".

values_to_json([]) ->
    "[]";
values_to_json([{Key, Value} | Values]) ->
    "[" ++ values_to_json(in_quotes(Value), Values) ++ "]".
values_to_json(Json, []) ->
    Json;
values_to_json(Json, [{Key, Value} | Values]) ->
    values_to_json(Json ++ ", " ++ in_quotes(Value), Values).

in_quotes(Message) ->
    ?QUOTE ++ Message ++ ?QUOTE.