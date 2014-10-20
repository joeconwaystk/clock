-module(clock_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        %% {URIHost, [{URIPath, Handler, Opts}]}
        {'_', [{"/time", route_time, []}]}
    ]),

    %% Name, NbAcceptors, TransOpts, ProtoOpts
    cowboy:start_http(clock_http_listener, 100,
        [{port, 8080}],
        [{env, [{dispatch, Dispatch}]}]
    ),
    clock_sup:start_link().

stop(_State) ->
    ok.
