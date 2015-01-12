%%%-------------------------------------------------------------------
%%% @author snow
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 十二月 2014 上午11:40
%%%-------------------------------------------------------------------
-module(schat_odbc).
-author("snow").

-include("schat_table.hrl").
-include_lib("stdlib/include/qlc.hrl").
%% API
-export([]).
-compile(export_all).


start() ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  mnesia:create_table(t_user, [{attributes, record_info(fields, t_user)},{type,set},{disc_copies,[node()]}]),
  mnesia:create_table(erlang_sequence, [{attributes, record_info(fields,
    erlang_sequence)},{type,set},{disc_copies,[node()]}]),
  ok.

do(Q) ->
  F = fun() -> qlc:e(Q) end,
  case mnesia:transaction(F) of
    {atomic, Val} ->
      NewVal = Val;
    _ ->
      NewVal = ""
  end,
  NewVal.

%% user

add_user(UserName, Password) ->
  F = fun() ->
    RowId = mnesia:dirty_update_counter(erlang_sequence, ?T_USER, 1),
    Row = #t_user{id = RowId, username = UserName, password = Password},
    mnesia:write(Row),
    RowId
  end,
  mnesia:transaction(F).

get_user_by_id(Id) ->
  F = fun() -> mnesia:read({?T_USER, Id}) end,
  mnesia:transaction(F).
get_user_by_up(UserName, Password) ->
  do(qlc:q([X || X <- mnesia:table(?T_USER),
    X#t_user.username =:= UserName,
    X#t_user.password =:= Password])).
get_user_by_un(UserName) ->
  do(qlc:q([X || X <- mnesia:table(?T_USER), X#t_user.username =:= UserName])).

