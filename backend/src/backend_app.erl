%%%-------------------------------------------------------------------
%% @doc backend public API
%% @end
%%%-------------------------------------------------------------------

-module(backend_app).

-behaviour(application).

-export([start/2, stop/1]).

%Register routes
start(_StartType, _StartArgs) ->
    %% Create the proxy storing interface
    DhtNode = spawn(dht_node, main, []),

    %%Put routing in place with store as init data
    Dispatch = cowboy_router:compile([
        {'_', [{"/[...]", dht_http_handler, [{dht_node, DhtNode}]}]}
    ]),

    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 2938}],
        #{env => 
            #{dispatch => Dispatch},
        middlewares =>
            [cowboy_router,
            ca_cowboy_middleware,
            cowboy_handler]}
    ),
    backend_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
