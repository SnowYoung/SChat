%%%-------------------------------------------------------------------
%%% @author snow
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 一月 2015 下午1:49
%%%-------------------------------------------------------------------
-module(schat_sup).
-author("snow").

-behaviour(supervisor).
%% API
-export([init/1, start_link/0]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {
    ok,
    {
      {one_for_one, 50, 10},
      [
        % session 管理服务
        {schat_server, {schat_server, start_link, []}, permanent, brutal_kill, worker, [schat_server]},
        % 认证服务
        {schat_auth, {schat_auth, start_link, []}, permanent, brutal_kill, worker, [schat_auth]},
        % 注册服务
        {schat_register, {schat_register, start_link, []}, permanent, brutal_kill, worker, [schat_register]},
        % msg处理服务
        {handle_message, {handle_message, start_link, []}, permanent, brutal_kill, worker, [handle_message]},

        {schat_muc, {schat_muc, start_link, []}, permanent, brutal_kill, worker, [schat_muc]}
      ]
    }
  }.