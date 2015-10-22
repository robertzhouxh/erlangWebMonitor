%%% File: my_http_module.erl
%%% Author: Xuehao Zhou <robertzhouxh.github.io>
%%%
%%% Created: Wednesday, October 21 2015

-module(my_http_module).

-include("sm.hrl").

-export([my_http_handler/1]).

my_http_handler(Req) ->
    lager:info("~p:~p ===> process starting ...", [?MODULE, ?LINE]),

    {Action, _} = cowboy_req:binding(action, Req),
    {Method, _} = cowboy_req:method(Req),
    HasBody = cowboy_req:has_body(Req),

    case HasBody of
        true ->
           Payload = get_body(Req);
        false ->
           Payload = <<"">>
    end,

    lager:info("get http request body: ===========>  ~p ", [Payload]),

    %% do not use list_to_item to create atom dynamicly
    Methodlist = binary_to_list(Method),

    Body = case Action of
               <<"login">> when Methodlist =:= "POST" ->
                   lager:info("starting ~p process", [Action]),
                   case login_handler(Req, Payload) of
                       {ok, Resp} ->
                           Resp;
                       {error, Resp} ->
                           Resp
                   end;
               <<"logout">> when Methodlist =:= "GET" ->
                   lager:info("starting ~p process", [Action]),
                   <<"ok">>;
               <<"users">> when Methodlist =:= "GET" ->
                   lager:info("starting ~p process", [Action]),
                   <<"ok">>;
               <<"online">> when Methodlist =:= "GET" ->
                   lager:info("starting ~p process", [Action]),
                   <<"ok">>;
               _ ->
                   lager:info("Invalid Request For ~p", [Action]),
                   <<"ok">>
           end,

    {ok, #sm_response{status  = 200,
                      headers = [{<<"content-type">>, <<"text/plain">>}],
                      body    = Body,
                      cookies = []}}.


%% Handlers
login_handler(Req, Body) ->
    ok = cowboy_session_config:set(cookie_options, [{path, <<"/">>}, {domain, <<"localhost">>}]),
    ok = cowboy_session_config:set([
                                    {cookie_name, <<"sessionid">>},
                                    {expire, 86400}
                                   ]),

    {ok, _} = cowboy_session:set(<<"sessionid">>, "sessionidvalue", Req),
    {ok, <<"successfully">>}.


%% Internal apis
get_body(Req) ->
    get_body(Req, []).
get_body(Req, Body) ->
    case cowboy_req:body(Req) of
        {ok, Data, Req1} ->
            Body ++ Data;
        {more, Data, Req1} ->
            get_body(Req1, Body ++ Data)
    end.
