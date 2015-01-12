%%%-------------------------------------------------------------------
%%% @author snow
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. 一月 2015 下午3:22
%%%-------------------------------------------------------------------
-module(schat_parse).
-author("snow").
-include("schat_codec.hrl").

%% API
-export([parse_json/1, parse_packet/2,parse_packet/1]).

parse_json(Json) ->
  case jsx:is_json(Json) of
    true ->
      {ok, jsx:decode(Json)};
    false ->
      {error, no_a_json}
  end.

parse_packet(Packet) ->
  case parse_json(Packet) of
    {ok, Json} ->
      parse_packet(Json, #packet{origin_packet = Packet});
    {error, Reson} ->
      #packet{}
  end.

parse_packet([], P) ->
  P;
parse_packet([Item | Packet], P) ->
  case Item of
    {<<"from">>, From} ->
      NewP = P#packet{from = From};
    {<<"to">>, To} ->
      NewP = P#packet{to = To};
    {<<"type">>, Type} ->
      NewP = P#packet{type = Type};
    {<<"body">>, Body} ->
      NewP = P#packet{body = Body}
  end,
  parse_packet(Packet, NewP).