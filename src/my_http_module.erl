%%% File: my_http_module.erl
%%% Author: Xuehao Zhou <robertzhouxh.github.io>
%%%
%%% Created: Wednesday, October 21 2015

-module(my_http_module).

-include("sm.hrl").

-export([my_http_handler/1]).

my_http_handler(Req) ->
    {Action, Req2} = cowboy_req:binding(action, Req),
    {Method, Req3} = cowboy_req:method(Req2),
    HasBody = cowboy_req:has_body(Req3),

    case HasBody of
        true ->
           Payload = get_body(Req3);
        false ->
           Payload = <<"">>
    end,

    lager:info("get http request body: ===========>  ~p ", [Payload]),
    %% do not use list_to_item to create atom dynamically
    {Status, Body, Cookies, Headers} = enter_handlers(Action,
                                                      binary_to_list(Method),
                                                      Req3,
                                                      Payload
                                                     ),
    {ok, #sm_response{status  = Status,
                      headers = Headers,
                      body    = Body,
                      cookies = Cookies}}.

%% Handlers
enter_handlers(Action, Methodlist, Req, Payload) ->
    case Action of
        <<"login">> when Methodlist =:= "POST" ->
            lager:info("starting ~p process", [Action]),
            case login_handler(Req, Payload) of
                {ok, Resp} ->
                    {200,
                     Resp,
                     [#sm_cookie{name = <<"sessionid">>, value = <<"xxx">>, domain = <<"localhost">>, path = <<"/">>, max_age = 3600}],
                     [{<<"content-type">>, <<"application/json">>}]};
                {error, Resp} ->
                    redirect_to("/index.html")
            end;
        <<"logout">> when Methodlist =:= "GET" ->
            lager:info("starting ~p process", [Action]),
            {200, <<"ok">>, [], []};
        <<"users">> when Methodlist =:= "GET" ->
            lager:info("starting ~p process", [Action]),
            {200, <<"ok">>, [], []};
        <<"online">> when Methodlist =:= "GET" ->
            lager:info("starting ~p process", [Action]),
            {200, <<"ok">>, [], []};
        _ ->
            lager:info("Invalid Request For ~p and redirect to URL: ~s", [Action, "/"]),
            redirect_to("/")
    end.

login_handler(Req, Body) ->
    {ok, _} = cowboy_session:set(<<"robertzhouxh">>, "xxxxxxxxxxxxxxxxxxxxxxxx", Req),
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

%% {ok, Req2} = cowboy_req:reply(302, [{<<"Location">>, Location}], Req),
redirect_to(Location) ->
    {302, <<>>, [], [{<<"Location">>, Location}]}.
