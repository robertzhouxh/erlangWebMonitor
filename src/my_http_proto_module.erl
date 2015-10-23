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
                    redirect_to(Payload, "/index.html")
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
            redirect_to(Payload, "/")
    end.

login_handler(Req, Body) ->
    {ok, _} = cowboy_session:set(<<"robertzhouxh">>, "xxxxxxxxxxxxxxxxxxxxxxxx", Req),
    %% process the Body
    %% Erlang term() will be encoded into Json Object
    %% Reply = jsx:encode([{<<"library">>,<<"derp">>},{<<"awesome">>,<<"nerp">>},{<<"IsAwesome">>,<<"ME">>}]),
    Reply = [{<<"username">>,<<"robertzhouxh">>}, {<<"awesome">>,<<"yes">>}],
    {ok, Reply}.


%% {ok, Req2} = cowboy_req:reply(302, [{<<"Location">>, Location}], Req),
redirect_to(Reply, Location) ->
    {302, Reply, [], [{<<"Location">>, Location}]}.
