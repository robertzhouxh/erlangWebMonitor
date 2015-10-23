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

    {Status, Reply, Cookies, Headers} = enter_handlers(Action,
                                                      binary_to_list(Method),
                                                      Req3,
                                                      Decoded
                                                     ),
    {ok, Reply, #sm_response{status  = Status,
                      headers = Headers,
                      cookies = Cookies}}.

%% Handlers
enter_handlers(Action, Method, Req, Payload) ->
    case Action of
        <<"login">> when Method =:= "POST" ->
            lager:info("starting ~p process", [Action]),
            case login_handler(Req, Payload) of
                {ok, Resp} ->
                    {200,
                     Resp,
                     [#sm_cookie{name = <<"sessionid">>, value = <<"xxx">>, domain = <<"localhost">>, path = <<"/">>, max_age = 3600}],
                     [{<<"content-type">>, <<"application/json">>}]};
                {error, Resp} ->
                    {403,
                     Resp,
                     [],
                     [{<<"content-type">>, <<"application/json">>}]}

            end;
        <<"logout">> when Method =:= "GET" ->
            lager:info("starting ~p process", [Action]),
            {200, <<"ok">>, [], []};
        <<"users">> when Method =:= "GET" ->
            lager:info("starting ~p process", [Action]),
            {200, <<"ok">>, [], []};
        <<"online">> when Method =:= "GET" ->
            lager:info("starting ~p process", [Action]),
            {200, <<"ok">>, [], []};
        _ ->
            lager:info("Invalid Request For ~p and redirect to URL: ~s", [Action, "/"]),
            redirect_to(Payload, ?INDEX_URL)
    end.


%% Erlang term() will be encoded into Json Object:
%% jsx:encode([{<<"library">>,<<"derp">>},{<<"awesome">>,<<"nerp">>},{<<"IsAwesome">>,<<"ME">>}]);
login_handler(Req, Body) ->
    ok = cowboy_session_config:set(cookie_options, [{path, <<"/">>}, {domain, <<"localhost">>}]),
    ok = cowboy_session_config:set([
                                    {cookie_name, <<"sessionid">>},
                                    {expire, 86400}
                                   ]),
    [{<<"username">>, Username}, {<<"password">>, Password}] = Body,
    Src = binary_to_list(Username) ++ ":" ++ binary_to_list(Password),
    lager:info("~p:~p SRC: ~p", [?MODULE, ?LINE, Src]),

    {ok, Pwdhash} = hash_password("admin:pass"),
    lager:info("~p:~p Pwdhash: ~p", [?MODULE, ?LINE, Pwdhash]),

    case check_password(Src, Pwdhash) of
        true -> case cowboy_session:set(<<"sessionid">>, "xxx", Req) of
                    {ok, _} -> {ok, [{<<"msg">>, <<"Login Successfully!">>}]}
                end;
        false -> {error, [{<<"msg">>, <<"Login Failed!">>}]}
    end.

require_login(Req) ->
    case get_session(Req) of
        undefined -> true;
        Sessionid ->
            cowboy_session
    end.

get_session(Req) ->
    Cookies = cowboy_req:parse_cookies(Req),
    case lists:keyfind(<<"sessionid">>, 1, Cookies) of
        false -> undefined;
        {_, Sessionid} -> Sessionid
    end.

%% {ok, Req2} = cowboy_req:reply(302, [{<<"Location">>, Location}], Req),
redirect_to(Reply, Location) ->
    {302, Reply, [], [{<<"Location">>, Location}]}.

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
