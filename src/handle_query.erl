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
%% API
-export([process/2]).

process(Packet,Session)->
  Body = schat_codec:dec_query(Packet#packet.body),
  case Body#p_query.type of
    <<"user">>->
      handle_user_query(Body#p_query.key,Packet#packet.from,Session);
    _ ->
      ok
  end.
handle_user_query(Key,From,Session)->
  case Key of
    <<"all">> ->
      Users = schat_server:get_users(),
      U = parse_users(Users,[],From),
      Session ! {deliver,schat_codec:packet(From,<<"query">>,schat_codec:enc_query_user_all_reply(U))},
      ok;
    _ ->
      ok
  end.
parse_users([],Users,From)->
  Users;
parse_users([Item|Items],Users,From)->
  case Item of
    {user,From,_,_,_,_,_,_}->
      NewUsers = Users;
    {user,Id,Name,_,_,_,_,_}->
      NewUsers = [ [{id,Id},{name,Name}] | Users]
  end,
  parse_users(Items,NewUsers,From).
