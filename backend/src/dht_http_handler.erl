-module(dht_http_handler).

-export([init/2]).

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

%Default
init(Req0, State) ->
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        "{\"Message\" : \"Hello\"}",
        Req0),
    
    {ok, Req, State}.