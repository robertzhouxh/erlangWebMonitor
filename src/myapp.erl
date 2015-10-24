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

-export([start/0, env/1, env/2, is_running/1]).

%%------------------------------------------------------------------------------
%% @doc Start application.
%% @end
%%------------------------------------------------------------------------------
-spec start() -> ok | {error, any()}.
start() ->
    application:start(crypto),
    application:start(ranch),
    application:start(cowlib),
    application:start(cowboy),
    application:start(lagger),
    application:start(gproc),
    application:start(uuid),
    application:start(cowboy_session),
    application:start(bcrypt),

    application:start(emysql),
    application:start(eredis_pool),


    io:format("~p:~p starting~n", [?MODULE, ?LINE]),
    application:start(manager).

%%------------------------------------------------------------------------------
%% @doc Get environment
%% @end
%%------------------------------------------------------------------------------
-spec env(atom()) -> list().
env(Group) ->
    application:get_env(manager, Group, []).

-spec env(atom(), atom()) -> undefined | any().
env(Group, Name) ->
    proplists:get_value(Name, env(Group)).

%%------------------------------------------------------------------------------
%% @doc Is running?
%% @end
%%------------------------------------------------------------------------------
is_running(Node) ->
    case rpc:call(Node, erlang, whereis, [emqttd]) of
        {badrpc, _}          -> false;
        undefined            -> false;
        Pid when is_pid(Pid) -> true
    end.
