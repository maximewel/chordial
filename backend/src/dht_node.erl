-module(dht_node).

-export([init/2]).

%Store
init(Req=#{path := <<"/store">>}, State) ->
    %%Retreive number
    #{number := Number} = cowboy_req:match_qs([number], Req),
    io:format("Number : ~p~n", [Number]),

    %%Send it to store
    Store = lists:keyfind(store, 1, State).
    Store ! {store, self(), {Number, Number+10}}

    receive
        {store, success} ->
            Req = cowboy_req:reply(200,
            #{<<"content-type">> => <<"text/plain">>},
            <<["You stored number", Number]>>,
            Req).
    end

    {ok, Req, State};

%Look up
init(Req=#{path := <<"/lookup">>}, State) ->
    %%Retreive number
    #{number := Number} = cowboy_req:match_qs([number], Req),
    io:format("Number : ~p~n", [Number]),

    %%Look-up in store
    Store = lists:keyfind(store, 1, State).
    Store ! {lookup, self(), Number}

    receive
        {lookup, Finds} ->
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"text/plain">>},
                <<["Number stored in process: ", Number]>>,
                Req).
    end
    
    {ok, Req, State};

%Default
init(Req, State) ->
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        "\"Message\" : \"Hello\"",
        Req),
    
    {ok, Req, State}.