%%%-------------------------------------------------------------------
%%% @author snow
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 一月 2015 下午1:15
%%%-------------------------------------------------------------------
-module(schat_muc).
-author("snow").

-behaviour(gen_server).

%% API
-export([start_link/0, create_room/2, get_rooms/0,join_room/2,deliver/1]).

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

%%%===================================================================
%%% API
%%%===================================================================

create_room(Name, Creater) ->
  case gen_server:call(?MODULE, {create_room, Name, Creater}) of
    {ok, Room} ->
      start_room(Room);
    {error, Reason} ->
      ok
  end,
  ok.

deliver(Packet)->
  ok.

get_rooms() ->
  ok.

join_room(Id, Uid) ->
  gen_server:call(?MODULE,{join_room,Id,Uid}).
leave_room(Id, Uid) ->
  ok.

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


start_room(Room) ->
  case Room of
    {t_room, Id, Name, Creater, Level, MaxM, CTime, Type} ->
      RoomInfo = #room_info{id = Id, name = Name, creater = Creater, level = Level, maxm = MaxM, ctime = CTime, type = Type},
      {ok, Pid} = schat_muc_sup:start_room(RoomInfo),
      NewRoomInfo = RoomInfo#room_info{pid = Pid},
      ets:insert(?ETS_ROOMS, NewRoomInfo);
    _ ->
      io:format("error~n")
  end.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  ets:new(?ETS_ROOMS, [public, ordered_set, named_table, {keypos, #room_info.id}]),
  schat_odbc:start(),
  mnesia:force_load_table(?T_ROOM),
  schat_muc_sup:start_link(),
  Rooms = schat_odbc:get_rooms(),
  lists:foreach(fun(Room) ->
    start_room(Room)
  end, Rooms),
  {ok, #state{}}.


-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call({create_room, Name, Creater}, _From, State) ->
  case schat_odbc:create_room(Name, Creater) of
    {atomic, Room} ->
      Reply = {ok, Room};
    _ ->
      Reply = {error, create_fail}
  end,
  {reply, Reply, State};
handle_call({join_room,Rid,Uid},_From, State) ->
  case schat_odbc:get_room_user(Rid,Uid) of
      [] ->
        case schat_odbc:add_room_user(Rid,Uid) of
          {atomic,User} ->
            {ok,Room} =  get_room(Rid),
            Room#room_info.pid ! {user_join,User},
            Reply = {ok,joined};
          _ ->
            Reply = {error, join_fail}
        end;
      _ ->
        Reply = {ok,already_join}
  end,
  {reply,Reply,State};
handle_call({deliver,Packet},_From,State) ->
  case ets:lookup(?ETS_ROOMS,Packet#packet.room) of
    [] ->
      ok
  end,
  {reply,ok,State};
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
handle_info(_Info, State) ->
  {noreply, State}.

-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_room(Id)->
  case ets:lookup(?ETS_ROOMS,Id) of
    [Room] ->
      {ok,Room};
    _ ->
      {error,room_notfound}
  end.