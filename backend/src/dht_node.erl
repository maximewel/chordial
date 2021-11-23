-module(dht_node).

-export([init/2]).

%Store
init(Req=#{path := <<"/store">>}, State) ->
    #{number := Number} = cowboy_req:match_qs([number], Req),

    io:format("Number : ~p~n", [Number]),

    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        <<"You want to store a number">>,
        Req),
    
    {ok, Req, State};

%Look up
init(Req=#{path := <<"/lookup">>}, State) ->
    #{number := Number} = cowboy_req:match_qs([number], Req),

    io:format("Number : ~p~n", [Number]),

    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        <<"You want to lookup a number">>,
        Req),
    
    {ok, Req, State};

%Default
init(Req, State) ->
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        "\"Message\" : \"Hello\"",
        Req),
    
    {ok, Req, State}.