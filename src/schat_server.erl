%%%-------------------------------------------------------------------
%%% @author snow
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 十二月 2014 上午11:26
%%%-------------------------------------------------------------------
-module(schat_server).
-author("snow").

-define(TPC_OPTIONS, [binary, {packet, 0}, {reuseaddr, true}, {active, false}]).
-define(DEF_PORT, 9999).
-behaviour(gen_server).

%% API
-export([start_link/0, create_session/1, bind_session/1, logout/1,deliver/1,get_session/1,get_users/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

-include("schat.hrl").

%%%===================================================================
%%% API
%%%===================================================================

deliver(Packet)->
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

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  lager:info("schat_server started"),
  ets:new(?ETS_USERS, [public, ordered_set, named_table, {keypos, #user.id}]),
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call({create_session,Socket}, _From, State) ->
  {ok, Pid} = schat_session:start_link(Socket),
  {reply, Pid, State};
handle_call({remove_user, User}, _From, State) ->
  Key = User#user.id,
  io:format("user :~p,logout~n",[User]),
  ets:delete(?ETS_USERS, Key),
  User#user.session ! {stop},
  {reply, ok, State};
handle_call({get_session,Key},_From,State) ->
  case ets:lookup(?ETS_USERS,Key) of
    [Session] ->
      Reply = { ok,Session};
    _ ->
      Reply = {error,not_found}
  end,
  {reply, Reply,State};
handle_call({get_users},_From,State) ->
  Users = ets:tab2list(?ETS_USERS),
  {reply,Users,State};
handle_call({send_msg, Id,Msg}, _From, State) ->
  Key = ets:first(?ETS_USERS),
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


get_users()->
  gen_server:call(?MODULE,{get_users}).

create_session(Socket) ->
  gen_server:call(?MODULE, {create_session,Socket}).

bind_session(Session) ->
  io:format("bind session ~p~n",[Session]),
  ets:insert(?ETS_USERS, Session).
get_session(Id) ->
  gen_server:call(?MODULE,{get_session,Id}).

logout(Ref) ->
  gen_server:call(?MODULE, {remove_user, Ref}),
  ok.

%% %%broad CastMsg to all connected clientSessions
%% broadCastMsg(Msg) ->
%%   gen_server:call(?MODULE, {send_msg, Msg}).
%%
%% sendMsg(Key, Msg) ->
%%   case ets:lookup(?ETS_USERS, Key) of
%%     [User] ->
%%       io:format("Record found ~p~n", [User]),
%%       Pid = User#user.pid,
%%       %while send down we change msg type to dwmsg
%%       io:format("send smg to client_session ~p~n", [Pid]),
%%       Pid ! {dwmsg, Msg},
%%       Next = ets:next(?ETS_USERS, Key),
%%       sendMsg(Next, Msg);
%%     [] ->
%%       io:format("no clientinfo found~n")
%%   end
%%   ,
%%   ok
%% ;
%% sendMsg([], Msg) ->
%%   ok.

