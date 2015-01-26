%%%-------------------------------------------------------------------
%%% @author snow
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. 一月 2015 下午2:34
%%%-------------------------------------------------------------------

-author("snow").

-record(erlang_sequence,{name,seq}).

-record(t_user,{id,username,password}).
-record(t_room,{id,name,creater,level=1,maxm=50,ctime,type=1}).
-record(t_room_user,{id,rid,uid,jtime,type=1,state=1}).

-define(T_USER,t_user).
-define(T_ROOM,t_room).
-define(T_ROOM_USER,t_room_user).
