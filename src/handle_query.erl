%%%-------------------------------------------------------------------
%%% @author snow
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 一月 2015 下午1:01
%%%-------------------------------------------------------------------
-module(handle_query).
-author("snow").

-include("schat_codec.hrl").
-include("schat.hrl").
%% API
-export([process/1]).

process(Packet) ->
  Body = Packet#packet.body,
  case Body#p_query.type of
    <<"user">> ->
      Reply = handle_user_query(Body, Packet#packet.from);
    <<"muc">> ->
      Reply = handle_muc_query(Body, Packet#packet.from);
    _ ->
      Reply = ?QUERY_NOT_SUPPORT
  end,
  NewPacket = Packet#packet{to = Packet#packet.from, from = <<"server">>, body = Reply},
  schat_server:deliver(NewPacket).

handle_muc_query(Body, From) ->
  case Body#p_query.key of
    <<"all">> ->
      Rooms = schat_muc:get_rooms(),
      [{type, <<"success">>}, {data, parse_rooms(Rooms, [])}];
    <<"my">> ->
      Rooms = schat_muc:get_user_rooms(From),
      [{type, <<"success">>}, {data, parse_rooms(Rooms, [])}];
    <<"create">> ->
      case schat_muc:create_room(Body#p_query.fields#muc_room_fields.name, From) of
        {ok, Id} ->
          [{type, <<"success">>}, {id, Id}];
        _ ->
          [{type, <<"error">>}, {message, <<"">>}]
      end;
    _ ->
      ?QUERY_NOT_SUPPORT
  end.
handle_user_query(Body, From) ->
  case Body#p_query.key of
    <<"all">> ->
      Users = schat_server:get_users(),
      U = parse_users(Users, [], From),
      schat_codec:enc_query_user_all_reply(U);
    _ ->
      ?QUERY_NOT_SUPPORT
  end.

parse_rooms([], Rooms) ->
  Rooms;
parse_rooms([Item | Items], Rooms) ->
  case Item of
    {room_info, Id, Name, Creater, Level, MaxM, CTime, Type,_,_} ->
      NewRooms = [[{id, Id}, {name, Name}] | Rooms];
    {t_room, Id,Name,_,_,_,_,_} ->
      NewRooms = [[{id,Id},{name,Name}] | Rooms];
    _ ->
      NewRooms = Rooms
  end,
  parse_rooms(Items, NewRooms).

parse_users([], Users, From) ->
  Users;
parse_users([Item | Items], Users, From) ->
  case Item of
    {user, From, _, _, _, _, _, _} ->
      NewUsers = Users;
    {user, Id, Name, _, _, _, _, _} ->
      NewUsers = [[{id, Id}, {name, Name}] | Users]
  end,
  parse_users(Items, NewUsers, From).
