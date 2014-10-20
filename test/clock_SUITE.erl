-module(clock_SUITE).

-include_lib("common_test/include/ct.hrl").


-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).

-export([]).

all() ->
    % Add function names for each test function
    [].


init_per_suite(Config) ->

    % Add applications in the order they should be started for tests
    Applications = [crypto, gproc, jsx, ranch, pgsql, episcina, cowlib, cowboy, jsxn, sch, inets, wsh, clock],


    % Shut everything down to reload any config files
    lists:foreach(fun(App) ->
        application:stop(App)
    end, lists:reverse(Applications)),

    % Start necessary applications
    lists:foreach(fun(App) ->
        ok = application:start(App)
    end, Applications),

    Config.

end_per_suite(Config) ->
    ok.
