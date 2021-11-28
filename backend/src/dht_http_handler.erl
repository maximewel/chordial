-module(dht_http_handler).

-export([init/2]).

%Store
init(Req=#{path := <<"/store">>}, State) ->
    %%Retreive number
    #{number := BinaryNumber} = cowboy_req:match_qs([number], Req),
    Number = binary_to_integer(BinaryNumber),
    io:format("Number : ~p~n", [Number]),

    %%Send it to store
    {_, DhtNode} = lists:keyfind(dht_node, 1, State),
    DhtNode ! {store, self(), Number},

    receive
        {store, success} ->
            Req = cowboy_req:reply(200,
            #{<<"content-type">> => <<"text/plain">>},
            "You stored number" ++ integer_to_list(Number),
            Req)
    end,

    {ok, Req, State};

%Look up
init(Req=#{path := <<"/lookup">>}, State) ->
    %%Retreive number
    #{number := BinaryNumber} = cowboy_req:match_qs([number], Req),
    Number = binary_to_integer(BinaryNumber),
    io:format("Number : ~p~n", [Number]),

    %%Send it to store
    {_, DhtNode} = lists:keyfind(dht_node, 1, State),
    DhtNode ! {lookup, self(), Number},

    receive
        {lookup, Finds} ->
            io:format("Finds : ~p~n", [Finds]),
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"text/plain">>},
                "Number stored in process: " ++ lists:flatten(io_lib:format("~p", [Finds])),
                Req)
    end,
    
    {ok, Req, State};

%Default
init(Req, State) ->
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        "\"Message\" : \"Hello\"",
        Req),
    
    {ok, Req, State}.