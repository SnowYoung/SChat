%%%-------------------------------------------------------------------
%%% @author snow
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 一月 2015 上午11:07
%%%-------------------------------------------------------------------
-module(schat_muc_room).
-author("snow").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-include("schat.hrl").
-include("schat_table.hrl").
-include("schat_codec.hrl").
-record(state, {}).


-spec(start_link(Args :: term()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Param) ->
  gen_server:start_link(?MODULE,[Param],[]).

add_user(RoomUser)->
  case RoomUser of
    {t_room_user,Id,Rid,Uid,JTime,Type,State}->
      User = #room_user{id=Id,rid = Rid,uid=Uid,jtime = JTime,type = Type,state = State},
      io:format("user ~w joined the room~n",[User#room_user.id]),
      ets:insert(?ETS_ROOM_USER,User);
    _ ->
      ok
  end.

-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([Param]) ->
  % 创建 房间用户表
  ets:new(?ETS_ROOM_USER, [public, ordered_set, named_table, {keypos, #room_user.id}]),
  case is_record(Param,room_info) of
    true ->
      io:format("room ~p started~n",[Param#room_info.id]),
      ok;
    false ->
      io:format("room ~p started~n",[Param#room_info.id]),
      ok
  end,
  schat_odbc:start(),
  mnesia:force_load_table(?T_ROOM_USER),
  % 加载房间所有成员
  Users = schat_odbc:get_room_users(Param#room_info.id),
  lists:foreach(fun(User)-> add_user(User) end,Users),
  {ok, #state{}}.


-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
  {reply, ok, State}.


-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.


-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info({user_join,User},State) ->
  add_user(User),
  {noreply,State};
handle_info({deliver,Packet},State) ->
  Users = ets:tab2list(?ETS_ROOM_USER),
  lists:foreach(fun(User)->
    NewPacket = Packet#packet{to = User#room_user.uid},
    schat_server:deliver(NewPacket)
  end,Users),
  {noreply,State};
handle_info(_Info, State) ->
  {noreply, State}.

-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
