%%%-------------------------------------------------------------------
%%% @author snow
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 一月 2015 上午11:06
%%%-------------------------------------------------------------------
-module(schat_muc_sup).
-author("snow").

-behaviour(supervisor).

%% API
-export([start_link/0,start_room/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_room(Param) ->
  supervisor:start_child(schat_muc_sup,[Param]).

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).


init([]) ->
  RestartStrategy = simple_one_for_one,
  MaxRestarts = 50,
  MaxSecondsBetweenRestarts = 10,
  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
  {ok, {SupFlags, [{schat_muc_room, {schat_muc_room, start_link, []}, permanent, brutal_kill, worker, [schat_muc_room]}]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
