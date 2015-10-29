%%% File: my_http_module.erl
%%% Author: Xuehao Zhou <robertzhouxh.github.io>
%%%
%%% Created: Wednesday, October 21 2015

-module(my_http_proto_module).

-include("sm.hrl").

-export([my_http_proto_handler/2]).
-define(DB_INDEX, 2).
-define(TableName, pre_ucenter_members).

my_http_proto_handler(Decoded, Req) ->
    lager:info("~p:~p my_http_proto_handler:  ~p", [?MODULE, ?LINE, Decoded]),
    {Action, Req2} = cowboy_req:binding(action, Req),   
    lager:info("Action------------> ~n~p~n", [Action]),
    lager:info("Req---------------> ~n~p~n", [Req]),
    lager:info("Req2--------------> ~n~p~n", [Req2]),
    {Method, Req3} = cowboy_req:method(Req2),

    {Status, Reply, Cookies, Headers, ReqTail} = enter_handlers(Action,
                                                      binary_to_list(Method),
                                                      Req3,
                                                      Decoded
                                                     ),

    lager:info("~p:~p tobe sent to sm:  ~p", [?MODULE, ?LINE, Reply]),

    {ok, Reply, #sm_response{status  = Status, headers = Headers, cookies = Cookies}, ReqTail}.

%% Handlers
enter_handlers(Action, Method, Req, Payload) ->
    lager:info("Payload--------------> ~n~p~n", [Payload]), %% Payload is Decoded
    lager:info("Req -----------------> ~n~p~n", [Req]),
    case Action of
        <<"login">> when Method =:= "POST" ->
            lager:info("starting ~p process", [Action]),
            login_handler(Req, Payload);
        <<"logout">> when Method =:= "GET" ->
            lager:info("starting ~p process", [Action]),
            {200, <<"ok">>, [], [], Req};
        <<"users">> when Method =:= "GET" ->
            lager:info("starting ~p process", [Action]),
            users_handler(Req);
            %% case check_session(Req) of
            %%     {undefined, Req2} ->
            %%         lager:info("Reg2----------------> ~n~p~n", [Req2]),
            %%         redirect_to(Req2, <<>>, ?LOGIN_URL);
            %%     {SessionVal, Req2} ->
            %%         lager:info("Req2----------------> ~n~p~n", [Req2]),
            %%         users_handler(Req2)
            %% end;
        <<"online">> when Method =:= "GET" ->
            lager:info("starting ~p process", [Action]),
            online_handler(Req);
        _ ->
            lager:info("Invalid Request For ~p and redirect to URL: ~s", [Action, "/"]),
            redirect_to(Req, Payload, ?INDEX_URL)
    end.


%% Erlang term() will be encoded into Json Object:
%% jsx:encode([{<<"library">>,<<"derp">>},{<<"awesome">>,<<"nerp">>},{<<"IsAwesome">>,<<"ME">>}]);
login_handler(Req, [{<<"username">>, Username}, {<<"password">>, Password}]) ->
    ok = cowboy_session_config:set(cookie_options, [{path, <<"/">>}, {domain, <<"localhost">>}]),
    ok = cowboy_session_config:set([{cookie_name, <<"sessionid">>}, {expire, 86400}]),
    Src = binary_to_list(Username) ++ ":" ++ binary_to_list(Password),
    lager:info("~p:~p SRC: ~p", [?MODULE, ?LINE, Src]),

    {ok, Pwdhash} = hash_password("admin:pass"),  %% get encryption with salt 
    lager:info("~p:~p Pwdhash: ~p", [?MODULE, ?LINE, Pwdhash]),

    case check_password(Src, Pwdhash) of    %% verify the Password
        true -> case set_session(Req) of    %% after this step, Req3 adds a cookie, but 
                    {ok, Req3}->
                        lager:info("Req3--------------> ~n~p~n", [Req3]),
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

users_handler(Req) ->
    %% fetch the users from the mysql blablabla ...
    lager:info("~n~nReq in users ----------> ~p~n~n", [Req] ),
    UsersInfo = get_userinfo_from_mysql(), 
    %% lager:info("UserInfo ------------------> ~p~n~n", [UsersInfo]),
    Resp = UsersInfo,
    lager:info("Resp ------------> ~p~n~n", [Resp]),

    {200,
     Resp,
     [],
     [{<<"content-type">>, <<"application/json">>}],
     Req}.


online_handler(Req) ->
    lager:info("------------------------------------------"), 
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

%% ------------------------------------------------------------------------------------------------------
check_session(Req) ->
    lager:info("Req  in check_session ---> ~n~p~n", [Req]),
    {SessionId, Req2} = cowboy_req:cookie(<<"sessionid">>, Req), %% read the value of cookie
    lager:info("SessionId--------------> ~n~p~n", [SessionId]),
    lager:info("Req2-------------------> ~n~p~n",[Req2]),
    {SessionVal, Req3} = cowboy_session:get(SessionId, Req2),
    lager:info("~p:~p get the SessionId:~p ~n sessionVal:~p", [?MODULE, ?LINE, SessionId, SessionVal]),
    {SessionVal, Req3}.

set_session(Req) ->    %% generate a cookies in the Req 
    SessionId = uuid:v4(),
    lager:info("~p:~p get the SessionId:~p", [?MODULE, ?LINE, SessionId]),
    {ok, Req2} = cowboy_session:set(SessionId, "xxxxxxxxxxx", Req),
    %% eredis_pool:q({global, pool1}, ["SET", SessionId, SessionVal]),
    lager:info("~p:~p afterXXXXXXXXXXXXXXXXXXXXXXXX ~p", [?MODULE, ?LINE, Req2]),
    {ok, Req2}.

%% {ok, Req2} = cowboy_req:reply(302, [{<<"Location">>, Location}], Req),
redirect_to(Req, Reply, Location) ->
    {302, Reply, [], [{<<"Location">>, Location}], Req}.

%% @Password is the Hash of the right password
check_password(PasswordAttempt, PasswordHash) -> 
    lager:info("~p:~p check ... PasswordAttempt: ~p", [?MODULE, ?LINE, PasswordAttempt]),
    lager:info("~p:~p check ... PasswordHash ~p", [?MODULE, ?LINE, PasswordHash]),

    %% StoredPassword = erlang:binary_to_list(PasswordHash),
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
    eredis_pool:q({global, pool1}, ["select",?DB_INDEX]),
    {ok, SessionIdKeys} = eredis_pool:q({global, pool1},["keys", "*"]), 
    Sessions = lists:map(fun(SessionIdKey) -> 
                                 {ok, Sessioni} = eredis_pool:q({global, pool1}, ["HGETALL", SessionIdKey]), 
                                 Sessioni end, 
                         SessionIdKeys),
    lager:error("Sessions ########### ~n~p~n", Sessions), 
    {ok, Sessions}.
  
get_userinfo_from_mysql() ->
    {ok, UsersInfo} = emysql:select({?TableName, [regdate, email, username]}),
    UsersInfo. 

