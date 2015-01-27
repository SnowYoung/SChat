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
-export([route/1]).

route(Packet) ->
  io:format("route packet: ~p~n", [Packet]),
  case Packet#packet.type of
    <<"message">> ->
        case Packet#packet.body#p_msg.type of
          <<"chat">> ->
            handle_message:process(Packet);
          <<"groupChat">> ->
            schat_muc:deliver(Packet);
          _ ->
            io:format("message type not support")
        end;
    <<"query">> ->
        handle_query:process(Packet);
    _ ->
      ok
  end.

