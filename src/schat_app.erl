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
  schat_sup:start_link(),
  schat_listener:start(9999).
stop(_State) ->
  ok.
