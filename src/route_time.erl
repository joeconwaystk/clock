%%%-------------------------------------------------------------------
%%% @author joeconway
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Oct 2014 3:37 PM
%%%-------------------------------------------------------------------
-module(route_time).
-author("joeconway").

%% API
-export([init/2, terminate/3, process_request/1]).

init(Req, Opts) ->
    {stk_http, Req, Opts}.

terminate(_Reason, _Req, _State) ->
    ok.

process_request({Req, <<"POST">>, Body}) ->
    {405, [], <<>>};
process_request({Req, <<"GET">>}) ->
    {200, [], <<"hello">>};
process_request({Req, <<"PUT">>, Body}) ->
    {405, [], <<>>};
process_request({Req, <<"DELETE">>}) ->
    {405, [], <<>>}.

% API
