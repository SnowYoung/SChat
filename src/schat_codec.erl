%%%-------------------------------------------------------------------
%%% @author snow
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
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


enc_reg_resp(Id, Type, Message) ->
  [{id, to_binary(Id)}, {type, to_binary(Type)}, {message, to_binary(Message)}].

enc_reg_resp_err(Message) ->
  enc_reg_resp("0", "error", Message).

dec_message(Body)->
  dec_message(Body,#p_msg{}).
dec_message([],M) ->
  M;
dec_message([Item|Body],M) ->
  case Item of
    {<<"type">>,Type} ->
      NewM = M#p_msg{type = Type};
    {<<"content">>,Content} ->
      NewM = M#p_msg{content = Content}
  end,
dec_message(Body,NewM).

to_binary(X) ->
  case is_list(X) of
    true ->
      list_to_binary(X);
    false ->
      X
  end.