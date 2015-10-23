%%% File: my_http_module.erl
%%% Author: Xuehao Zhou <robertzhouxh.github.io>
%%%
%%% Created: Wednesday, October 21 2015

-module(my_http_proto_module).

-include("sm.hrl").

-export([my_http_proto_handler/2]).

my_http_proto_handler(Decoded, Req) ->
    lager:info("~p:~p my_http_proto_handler: +++++++++++++++++  ~p", [?MODULE, ?LINE, Decoded]),
    {Action, Req2} = cowboy_req:binding(action, Req),
    {Method, Req3} = cowboy_req:method(Req2),

    {Status, Reply, Cookies, Headers, ReqTail} = enter_handlers(Action,
                                                      binary_to_list(Method),
                                                      Req3,
                                                      Decoded
                                                     ),
    {ok, Reply, #sm_response{status  = Status, headers = Headers, cookies = Cookies}, ReqTail}.

%% Handlers
enter_handlers(Action, Method, Req, Payload) ->
    case Action of
        <<"login">> when Method =:= "POST" ->
            lager:info("starting ~p process", [Action]),
            login_handler(Req, Payload);
        <<"logout">> when Method =:= "GET" ->
            lager:info("starting ~p process", [Action]),
            {200, <<"ok">>, [], []};
        <<"users">> when Method =:= "GET" ->
            lager:info("starting ~p process", [Action]),
            case check_session(Req) of
                {undefined, Req2} ->
                    {200, <<"ok">>, [], [], Req2};
                {SessionVal, Req2} ->
                    users_handler(Req2)
            end;
        <<"online">> when Method =:= "GET" ->
            lager:info("starting ~p process", [Action]),
            {200, <<"ok">>, [], []};
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

    {ok, Pwdhash} = hash_password("admin:pass"),
    lager:info("~p:~p Pwdhash: ~p", [?MODULE, ?LINE, Pwdhash]),

    case check_password(Src, Pwdhash) of
        true -> case set_session(Req) of
                    {ok, Req3}->
                        {200,
                         [{<<"msg">>, <<"Login successfully!">>}],
                         [],
                         [{<<"content-type">>, <<"application/json">>}],
                         Req3}
                end;
        false -> {401,
                  [{<<"msg">>, <<"Login Failed!">>}],
                  [],
                  [{<<"content-type">>, <<"application/json">>}],
                  Req}
    end.


users_handler(Req) ->
    %% fetch the users from the mysql blablabla ...
    Resp = [{<<"username">>, <<"alice">>}, {<<"username">>, <<"bob">>}, {<<"username">>, <<"charlie">>}],
    {200,
     Resp,
     [],
     [{<<"content-type">>, <<"application/json">>}],
     Req}.



%% ------------------------------------------------------------------------------------------------------
check_session(Req) ->
    {SessionId, Req2} = cowboy_req:cookie(<<"sessionid">>, Req),
    {SessionVal, Req3} = cowboy_session:get(SessionId, Req2),
    lager:info("~p:~p get the SessionId:~p ~n sessionVal:~p", [?MODULE, ?LINE, SessionId, SessionVal]),
    {SessionVal, Req3}.


set_session(Req) ->
    SessionId = uuid:v4(),
    lager:info("~p:~p get the SessionId:~p", [?MODULE, ?LINE, SessionId]),
    {ok, Req2} = cowboy_session:set(SessionId, "xxxxxxxxxxx", Req),
    lager:error("~p:~p afterXXXXXXXXXXXXXXXXXXXXXXXX ~p", [?MODULE, ?LINE, Req2]),
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
