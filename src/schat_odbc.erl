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
  mnesia:create_table(t_user, [{attributes, record_info(fields, t_user)}, {type, set}, {disc_copies, [node()]}]),
  mnesia:create_table(t_room, [{attributes, record_info(fields, t_room)}, {type, set}, {disc_copies, [node()]}]),
  mnesia:create_table(t_room_user, [{attributes, record_info(fields, t_room_user)}, {type, set}, {disc_copies, [node()]}]),
  mnesia:create_table(erlang_sequence, [{attributes, record_info(fields,
    erlang_sequence)}, {type, set}, {disc_copies, [node()]}]),
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

create_room(Name, Creater) ->
  F = fun() ->
    RowId = mnesia:dirty_update_counter(erlang_sequence, ?T_ROOM, 1),
    Row = #t_room{id = RowId, name = Name, creater = Creater, ctime = os:timestamp()},
    mnesia:write(Row),
    Row
  end,
  mnesia:transaction(F).
get_rooms() ->
  do(qlc:q([X || X <- mnesia:table(?T_ROOM)])).

add_room_user(Rid, Uid) ->
  F = fun() ->
    RowId = mnesia:dirty_update_counter(erlang_sequence, ?T_ROOM_USER, 1),
    Row = #t_room_user{id = RowId, uid = Uid, rid = Rid, jtime = os:timestamp()},
    mnesia:write(Row),
    Row
  end,
  mnesia:transaction(F).
get_room_users(Rid) ->
  do(qlc:q([X || X <- mnesia:table(?T_ROOM_USER), X#t_room_user.rid =:= Rid])).
get_room_user(Rid, Uid) ->
  do(qlc:q([X || X <- mnesia:table(?T_ROOM_USER), X#t_room_user.rid =:= Rid, X#t_room_user.uid =:= Uid])).

get_user_rooms(Uid) ->
  do(qlc:q([Y || X<- mnesia:table(?T_ROOM_USER), X#t_room_user.uid =:= Uid,
                 Y<- mnesia:table(?T_ROOM), Y#t_room.id =:= X#t_room_user.rid])).