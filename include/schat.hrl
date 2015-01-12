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

-record(message,{type,from,content}).

-record(client_session,{user=#user{},state=no_auth,groups}).

-define(MANAGER_CLIENTS, clients_manager).

-define(ETS_USERS,ets_users).
