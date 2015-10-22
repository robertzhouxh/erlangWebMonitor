
%%% File: sm_protocol_payload.erl
%%% Author: Xuehao Zhou <robertzhouxh.github.io>
%%%
%%% Created: Thursday, October 22 2015

-module(sm_protocol_payload).
-author("robertzhouxh").

-behaviour(sm_protocol).

-export([supports_format/1, encode/2, decode/2]).

supports_format(Format) ->
    case Format of
        binary -> true;
        json -> true;
        text -> true;
        _ -> false
    end.

encode(Data, binary) ->
    term_to_binary(Data);
encode(Data, json) ->
    jsx:encode(Data);
encode(Data, text) ->
    Data.


decode(Data, binary) ->
    binary_to_term(Data, [safe]);
decode(Data, json) ->
    jsx:decode(Data);
decode(Data, text) ->
    Data.
