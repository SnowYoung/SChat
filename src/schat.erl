%%%-------------------------------------------------------------------
%%% @author snow
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. 十二月 2014 下午5:26
%%%-------------------------------------------------------------------
-module(schat).
-author("snow").
-export([start/0]).

start() ->
  application:start(systax_tools),
  application:start(goldrush),
  application:start(lager),
  application:start(jsx),
  application:start(schat_app).