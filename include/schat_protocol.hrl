%%%-------------------------------------------------------------------
%%% @author snow
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 十二月 2014 下午4:47
%%%-------------------------------------------------------------------
-author("snow").

-define(SCHAT_MSG(From,To,Msg),list_to_binary("\"from\":"++From++"\",to\":"++To++"\"type\":\"msg\",\"body\":"++Msg)).
