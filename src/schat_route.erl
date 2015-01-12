%%%-------------------------------------------------------------------
%%% @author snow
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 一月 2015 上午11:39
%%%-------------------------------------------------------------------
-module(schat_route).
-author("snow").

-include("schat_codec.hrl").
-export([route/1]).

route(Packet) ->
  io:format("route packet: ~p~n", [Packet]),
  case Packet#packet.type of
    <<"message">> ->
        handle_message:process(Packet);
    _ ->
      ok
  end.
