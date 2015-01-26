%%%-------------------------------------------------------------------
%%% @author snow
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%% 协议的封包与解包
%%% @end
%%% Created : 08. 一月 2015 下午3:29
%%%-------------------------------------------------------------------
-module(schat_codec).
-author("snow").

-include("schat_codec.hrl").
-compile(export_all).
%% API


packet(From, To, Type, Body) ->
  jsx:encode([{from, to_binary(From)}, {to, to_binary(To)}, {type, to_binary(Type)}, {body, Body}]).
packet(To, Type, Body) ->
  packet("server", To, Type, Body).


enc_packet(Packet) ->
  case is_record(Packet, packet) of
    true ->
      case Packet#packet.type of
        <<"message">> ->
          Body = enc_message(Packet#packet.body);
        <<"query">> ->
          Body = [];
        _ ->
          Body = [],
          ok
      end,
      jsx:encode([
        {from,to_binary(Packet#packet.from)},
        {to,to_binary(Packet#packet.to)},
        {type,to_binary(Packet#packet.type)},
        {body,Body},
        {room,to_binary(Packet#packet.room)},
        {node,to_binary(Packet#packet.node)},
        {time,to_binary(Packet#packet.time)}
      ]);
    false ->
      error
  end.

dec_json(Json) ->
  case jsx:is_json(Json) of
    true ->
      {ok, jsx:decode(Json)};
    false ->
      {error, no_a_json}
  end.

dec_packet(Packet) ->
  case dec_json(Packet) of
    {ok, Json} ->
      DecPacket =  dec_packet(Json, #packet{origin_packet = Packet}),
      case DecPacket#packet.type of
        <<"message">> ->
          Body = dec_message(DecPacket#packet.body);
        _ ->
          Body = DecPacket#packet.body
      end,
      DecPacket#packet{body = Body};
    {error, Reson} ->
      #packet{}
  end.

dec_packet([], P) ->
  P;
dec_packet([Item | Packet], P) ->
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
  dec_packet(Packet, NewP).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% message codec
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dec_message(Body) ->
  dec_message(Body, #p_msg{}).
dec_message([], M) ->
  M;
dec_message([Item | Body], M) ->
  case Item of
    {<<"type">>, Type} ->
      NewM = M#p_msg{type = Type};
    {<<"content">>, Content} ->
      NewM = M#p_msg{content = Content};
    {<<"messageType">>, MessageType} ->
      NewM = M#p_msg{messageType = MessageType};
    _ ->
      NewM = M
  end,
  dec_message(Body, NewM).

enc_message(Message) ->
  case is_record(Message, p_msg) of
    true ->
      [
        {type,to_binary(Message#p_msg.type)},
        {content,to_binary(Message#p_msg.content)},
        {messageType,to_binary(Message#p_msg.messageType)}
      ];
    false ->
      error
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
enc_reg_resp(Id, Type, Message) ->
  [{id, to_binary(Id)}, {type, to_binary(Type)}, {message, to_binary(Message)}].

enc_reg_resp_err(Message) ->
  enc_reg_resp("0", "error", Message).

dec_query(Body) ->
  dec_query(Body, #p_query{}).
dec_query([], M) ->
  M;
dec_query([Item | Body], M) ->
  case Item of
    {<<"type">>, Type} ->
      NewM = M#p_query{type = Type};
    {<<"key">>, Key} ->
      NewM = M#p_query{key = Key};
    _ ->
      NewM = M
  end,
  dec_query(Body, NewM).

enc_query_user_all_reply(Users) ->
  [{type, <<"success">>}, {data, Users}].


to_binary(X) ->
  case is_list(X) of
    true ->
      list_to_binary(X);
    false ->
      X
  end.