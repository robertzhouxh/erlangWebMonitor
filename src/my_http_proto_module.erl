%%% File: my_http_module.erl
%%% Author: Xuehao Zhou <robertzhouxh.github.io>
%%%
%%% Created: Wednesday, October 21 2015

-module(my_http_proto_module).

-include("sm.hrl").

-export([my_http_proto_handler/2]).

%% Information of databases
-define(RDDB_INDEX, 2).            %% Index of redis databases
-define(MSQL_USER_TAB, pre_ucenter_members).  %% Table that stores the Information of users

my_http_proto_handler(Decoded, Req) ->
    lager:info("~p:~p my_http_proto_handler Decoded:  ~p", [?MODULE, ?LINE, Decoded]),
    {Action, Req2} = cowboy_req:binding(action, Req),
    {Method, Req3} = cowboy_req:method(Req2),

    {Status, Reply, Cookies, Headers, ReqTail} = enter_handlers(Action,
                                                      binary_to_list(Method),
                                                      Req3,
                                                      Decoded
                                                     ),
    %% lager:info("Req3 ---------------> ~n~p~n", [Req3]),
    lager:info("~p:~p tobe sent to sm:  ~p", [?MODULE, ?LINE, Reply]),

    {ok, Reply, #sm_response{status  = Status, headers = Headers, cookies = Cookies}, ReqTail}.

%% Handlers
enter_handlers(Action, Method, Req, Payload) ->
    case Action of
        <<"login">> when Method =:= "POST" ->
            lager:info("starting ~p process", [Action]),
            login_handler(Req, Payload);
        <<"logout">> when Method =:= "GET" ->
            lager:info("starting ~p process", [Action]),
            logout_handler(Req);
        <<"users">> when Method =:= "GET" ->
            lager:info("starting ~p process", [Action]),
            case check_session(Req) of
                {undefined, Req2} ->
                    redirect_to(Req2, <<>>, ?LOGIN_URL);
                {_SessionVal, Req2} ->
                    users_handler(Req2)
            end;
        <<"online">> when Method =:= "GET" ->
            lager:info("starting ~p process", [Action]),
            case check_session(Req) of
                {undefined, Req2} ->
                    redirect_to(Req2, <<>>, ?LOGIN_URL);
                {_SessionVal, Req2} ->
                    online_handler(Req2)
            end;
        <<"devices">> when Method =:= "GET" ->
            lager:info("starting ~p process", [Action]),
            case check_session(Req) of
                {undefined, Req2} ->
                    redirect_to(Req2, <<>>, ?LOGIN_URL);
                {_SessionVal, Req2} ->
                    devices_handler(Req2)
            end;
        _ ->
            lager:info("Invalid Request For ~p and redirect to URL: ~s", [Action, "/"]),
            redirect_to(Req, Payload, ?INDEX_URL)

    end.


%% Erlang term() will be encoded into Json Object:
%% jsx:encode([{<<"library">>,<<"derp">>},{<<"awesome">>,<<"nerp">>},{<<"IsAwesome">>,<<"ME">>}]);
login_handler(Req, [{<<"username">>, Username}, {<<"password">>, Password}]) ->
    ok = cowboy_session_config:set(cookie_options, [{path, <<"/">>}]),
    ok = cowboy_session_config:set([{cookie_name, <<"sessionid">>}, {expires, 86400}]),
    Src = binary_to_list(Username) ++ ":" ++ binary_to_list(Password),
    lager:info("~p:~p SRC: ~p", [?MODULE, ?LINE, Src]),
    %% {ok, Pwdhash} = hash_password("admin:pass"),
    {ok, S} = file:open("../files/auth.dat", read),
    Pwdhash = io:get_line(S, ''),
    lager:info("~p:~p Pwdhash: ~p", [?MODULE, ?LINE, Pwdhash]),

    case check_password(Src, Pwdhash) of    %% verify the Password
        true -> case set_session(Req) of
                    {ok, Req3}->
                        %% lager:info("~p:~p Req3 ==============>  ~p", [?MODULE, ?LINE, Req3]),
                        {200,
                         [{<<"msg">>, <<"Login successfully!">>}],
                         [],
                         [{<<"content-type">>, <<"application/json">>}],
                         Req3}
                end;
        false -> {401,                                   %% Status
                  [{<<"msg">>, <<"Login Failed!">>}],    %% Reply
                  [],                                    %% Cookie 
                  [{<<"content-type">>, <<"application/json">>}], %% Headers
                  Req}                                            %% ReqTail
    end.

logout_handler(Req) ->
    case cowboy_session:expire(Req) of
        {ok, Req2} ->
            {200, [{<<"msg">>, <<"Logout!">>}], [], [], Req2}
    end.

users_handler(Req) ->
    %% fetch the users from the mysql blablabla ...
    UsersInfo = get_userinfo_from_mysql(),
    Resp = UsersInfo,

    {200,
     Resp,
     [],
     [{<<"content-type">>, <<"application/json">>}],
     Req}.

online_handler(Req) ->
    Resp = case get_session_from_redis() of
               {ok, [Record]} ->
                   lager:info("get from the redis server with record:  ~p~n", [Record]),
                   Record;
               {ok, Tuple} ->
                   lager:info("get from the redis server with record:  ~p~n", [Tuple]),
                   Tuple;
               _ ->
                   lager:info("-------can not find [key] in redis ----------------")
           end,
    {200,
     Resp,
     [],
     [{<<"content-type">>, <<"application/json">>}],
     Req}.

devices_handler(Req) ->
    DevicesInfo = get_devices_from_mongo(),
    Resp = DevicesInfo,
    lager:info("Devices msg ==========> ~n~p", [Resp]),
    {200,
     Resp,
     [],
     [{<<"content-type">>, <<"application/json">>}],
     Req}.

%% ------------------------------------------------------------------------------------------------------
check_session(Req) ->
    {SessionId, Req2} = cowboy_req:cookie(<<"sessionid">>, Req), %% read the value of cookie
    {SomeVal, Req3} = cowboy_session:get(<<"somekey">>, Req2),
    %% lager:error("~p:~p get the SessionId:~p ~n sessionVal:~p", [?MODULE, ?LINE, <<"somekey">>, SomeVal]),
    {SomeVal, Req3}.

%% generate a cookies in the Req
set_session(Req) ->
    Key = <<"somekey">>,
    Val = <<"someval">>,
    {ok, Req2} = cowboy_session:set(Key, Val, Req),
    {ok, Req2}.

redirect_to(Req, Reply, Location) ->
    {302, Reply, [], [{<<"Location">>, Location}], Req}.

%% @Password is the Hash of the right password
check_password(PasswordAttempt, PasswordHash) ->
    StoredPassword = PasswordHash,
    lager:info("~p:~p check ... StoredPassword ~p", [?MODULE, ?LINE, StoredPassword]),
    compare_password(PasswordAttempt, StoredPassword).


compare_password(PasswordAttempt, PasswordHash) ->
    {ok, PasswordHash} =:= bcrypt:hashpw(PasswordAttempt, PasswordHash).

%% On success, returns {ok, Hash}.
hash_password(Password)->
    {ok, Salt} = bcrypt:gen_salt(),
    bcrypt:hashpw(Password, Salt).

replvar(AuthSql, Username) ->
    re:replace(AuthSql, "%u", Username, [global, {return, list}]).


get_session_from_redis() ->
    eredis_pool:q({global, pool1}, ["select",?RDDB_INDEX]),
    {ok, SessionIdKeys} = eredis_pool:q({global, pool1},["keys", "*"]),
    Sessions = lists:map(fun(SessionIdKey) ->
                                 {ok, Sessioni} = eredis_pool:q({global, pool1}, ["HGETALL", SessionIdKey]),
                                 Sessioni end,
                         SessionIdKeys),
    {ok, Sessions}.


get_userinfo_from_mysql() ->
    {ok, UsersInfo} = emysql:select({?MSQL_USER_TAB, [regdate, email, username]}),
    UsersInfo.


get_devices_from_mongo() ->
    Database =  <<"production">>,
    {ok, Connection} = mongo:connect ([{database, Database}]),
    Collection = <<"device">>,

    DayBeginTimeYMDHMS = {erlang:date(), {0,0,0}},
    DayBeginTimeStamp = calendar:datetime_to_gregorian_seconds(DayBeginTimeYMDHMS) - calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}),
    lager:info("DayBeginTimeStamp ============> ~p~n", [DayBeginTimeStamp]),

    %% Selector based on https://github.com/comtihon/mongodb-erlang/issues/52
    SelectorAllDevs = {},
    SelectorPublic = {<<"isPublic">>, true},
    SelectorDayReg = {<<"created_at">>, {'$gte', DayBeginTimeStamp}},
    SelectorNewAndPublic = {'$and', [{<<"created_at">>, {'$gte', DayBeginTimeStamp}},{<<"isPublic">>, true}]},

    NumOfTatalDev = mongo:count(Connection, Collection, SelectorAllDevs),
    NumOfPubDev = mongo:count(Connection, Collection, SelectorPublic),
    NumNewRegDev = mongo:count(Connection, Collection, SelectorDayReg),
    NumNewAndPub = mongo:count(Connection, Collection, SelectorNewAndPublic),

    Devices =[{total_devs, NumOfTatalDev},
              {public_devs, NumOfPubDev}, 
              {today_new_devs, NumNewRegDev},
              {today_new_pub_devs, NumNewAndPub}],
    Devices.
