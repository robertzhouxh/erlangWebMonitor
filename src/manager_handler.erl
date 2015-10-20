%%%-----------------------------------------------------------------------------
%%% Copyright (c) 2015-2016 eMQTT.IO, All Rights Reserved.
%%%
%%%-----------------------------------------------------------------------------
%%% @doc
%%% molmc network plateform monitor handler
%%%
%%% @end
%%%-----------------------------------------------------------------------------

-module(manager_handler).

-user("robertzhouxh@gmail.com").

-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).

init(_, Req, _Opts) ->
    {ok, Req, #state{}}.

handle(Req, State=#state{}) ->
    %% 可能是文档没有跟上版本升级，cowboy官网误写为 Req2 = cowboy_....
    {ok, Req2} = cowboy_req:reply(200,
                                  [{<<"content-type">>, <<"text/plain">>}],
                                  <<"Hello Erlang!">>,
                                  Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.
