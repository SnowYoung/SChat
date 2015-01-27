%%%-------------------------------------------------------------------
%%% @author snow
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 十二月 2014 下午2:07
%%%-------------------------------------------------------------------
-author("snow").

-record(packet,{id=0,from=0,to=0,body,type=auth,node=self,room=0,time=0,origin_packet,packet}).

-record(packet_reg,{id,username,password}).
-record(packet_auth,{id,username,password}).

-record(p_msg,{type,content,messageType}).
-record(p_query,{type,key,value,fields}).

-record(muc_room_fields,{name}).

-define(QUERY_NOT_SUPPORT,[{warning,<<"query not support">>}]).