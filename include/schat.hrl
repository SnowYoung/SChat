%%%-------------------------------------------------------------------
%%% @author snow
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 十二月 2014 下午5:40
%%%-------------------------------------------------------------------
-author("snow").

-record(user,{id,name,token,mode,client,socket,session}).

-record(room_info, {id,name,creater,level=1,maxm=50,ctime,type=1,mnum=0,pid}).
-record(room_user, {id,rid,uid,jtime,type,state}).

-record(client_session,{user=#user{},state=no_auth,groups}).

-define(ETS_USERS,ets_users).
-define(ETS_ROOMS, ets_rooms).
-define(ETS_ROOM_USER,ets_room_user).

