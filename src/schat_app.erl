%%%-------------------------------------------------------------------
%%% @author snow
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 十二月 2014 下午12:44
%%%-------------------------------------------------------------------
-module(schat_app).
-author("snow").
-behaviour(application).


%% API
-export([start/2, stop/1]).

start(_Type, _Args) ->
  %启动监听树
  schat_sup:start_link(),
  %启动端口监听
  schat_listener:start(9999).
stop(_State) ->
  ok.
