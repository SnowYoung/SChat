%%%-------------------------------------------------------------------
%%% @author snow
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 十二月 2014 下午2:07
%%%-------------------------------------------------------------------
-author("snow").

-record(packet,{from,to,body,type,origin_packet}).

-record(packet_reg,{id,username,password}).
-record(packet_auth,{id,username,password}).

-record(p_msg,{type,content}).