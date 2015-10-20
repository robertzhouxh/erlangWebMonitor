%%%-----------------------------------------------------------------------------
%%% Copyright (c) 2015-2016 eMQTT.IO, All Rights Reserved.
%%%
%%%-----------------------------------------------------------------------------
%%% @doc
%%% molmc network plateform monitor start entry
%%%
%%% @end
%%%-----------------------------------------------------------------------------

-module(myapp).

-user("robertzhouxh@gmail.com").

-export([start/0]).

start() ->
  ok = application:start(crypto),
  ok = application:start(ranch),
  ok = application:start(cowlib),
  ok = application:start(cowboy),

  io:format("~p:~p starting~n", [?MODULE, ?LINE]),
  ok = application:start(manager).
