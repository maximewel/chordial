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
        {lookup, Finds} ->
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"text/plain">>},
                "Values stored for key= " ++ Key ++ ": " ++ lists:flatten(io_lib:format("~p", [Finds])),
                Req0)
    end,
    
    {ok, Req, State};

%Look up
init(Req0=#{path := <<"/state">>}, State) ->
    %%Retreive kea/value
    io:format("Fetching DHT state~n"),

    %%Send it to store
    {_, DhtNode} = lists:keyfind(dht_node, 1, State),
    DhtNode ! {state, self()},

    receive
        {state_finished, DHT_state} ->
            io:format("Nodes to json: ~p~n", [state_to_json(DHT_state)]),
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


%%% Helpers %%%
state_to_json([FirstNode | States]) ->
    io:format("First node : ~p~n", [FirstNode]),
    io:format("To json : ~p~n", [node_to_json(FirstNode)]),

    "{ " ++ in_quotes("nodes") ++ " : [" ++
        state_to_json(node_to_json(FirstNode), States) ++ 
    "]}".

state_to_json(Json, []) -> Json;
state_to_json(Json, [Node | States]) ->
    state_to_json(Json ++ "," ++ node_to_json(Node), States).

node_to_json(Node) ->
    {{identity, Identity}, {fingers, Fingers}, {storage, Storage}} = Node,
    "{" ++
        identity_to_json(Identity)
        ++
    "}".

identity_to_json({NodeID, NodeName}) ->
    in_quotes("id") ++ " : " ++ in_quotes(atom_to_list(NodeID)) ++ ","
    ++ in_quotes("node_name") ++ " : " ++ in_quotes(atom_to_list(NodeName)).

fingers_to_json() ->
    .

storage_to_json() ->
    .

in_quotes(Message) ->
    ?QUOTE ++ Message ++ ?QUOTE.