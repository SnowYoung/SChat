%%%-------------------------------------------------------------------
%%% @author snow
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%% 数据包路由
%%% @end
%%% Created : 04. 一月 2015 上午11:39
%%%-------------------------------------------------------------------
-module(schat_route).
-author("snow").

-include("schat_codec.hrl").
-export([route/2]).

route(Packet,Session) ->
  io:format("route packet: ~p~n", [Packet]),
  case Packet#packet.type of
    <<"message">> ->
        handle_message:process(Packet);
    <<"query">> ->
        handle_query:process(Packet,Session);
    _ ->
      ok
  end.

