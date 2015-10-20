%%%-----------------------------------------------------------------------------
%%% Copyright (c) 2015-2016 eMQTT.IO, All Rights Reserved.
%%%
%%%-----------------------------------------------------------------------------
%%% @doc
%%% molmc network plateform monitor
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(manager_app).

-author("robertzhouxh@gmail.com").

-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([{'_',[
                                            {"/", manager_handler, []}
                                           ]}]),
    cowboy:start_http(my_http_listener, 100, [{port, 8080}],
                      [{env, [{dispatch, Dispatch}]}]
                     ),

    io:format("~p:~p starting ~p~n", [?MODULE, ?LINE, 8080]),

    manager_sup:start_link().

stop(_State) ->
    ok.
