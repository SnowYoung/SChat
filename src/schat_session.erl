%%%-------------------------------------------------------------------
%%% @author snow
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. 一月 2015 下午6:29
%%%-------------------------------------------------------------------
-module(schat_session).
-author("snow").

-behaviour(gen_fsm).
-include("schat.hrl").
-include("schat_codec.hrl").

%% API
-export([start_link/0, deliver/1]).

%% gen_fsm callbacks
-export([init/1,
  handle_event/3,
  handle_sync_event/4,
  handle_info/3,
  terminate/3,
  code_change/4,
  wait_auth_or_register/2,
  active/2,
  stop/2]).

-define(SERVER, ?MODULE).


%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() -> {ok, pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, StateName :: atom(), StateData :: #client_session{}} |
  {ok, StateName :: atom(), StateData :: #client_session{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, wait_auth_or_register, #client_session{}}.

deliver(Packet) ->

  ok.

wait_auth_or_register(_Event, State) ->
  case _Event of
    {packet, Packet} ->
      case Packet#packet.type of
        <<"register">> ->
          case schat_register:process(Packet) of
            {ok, Id} ->
              NewUser = State#client_session.user#user{id = Id},
              NewState = State#client_session{user = NewUser},
              schat_server:bind_session(NewUser),
              StateName = active,
              %gen_tcp:send(State#client_session.user#user.socket, jsx:encode([{from, <<"server">>}, {to, <<"a">>}]));
              State#client_session.user#user.session ! {deliver, schat_codec:packet(Id,"register",schat_codec:enc_reg_resp(Id,"success",""))};
            _ ->
              io:format("register failed"),
              State#client_session.user#user.session ! {deliver, schat_codec:packet("0","register",schat_codec:enc_reg_resp_err("username already use"))},
              NewState = State,
              StateName = wait_auth_or_register
          end;
        <<"auth">> ->
          case schat_auth:process(Packet) of
            {ok, Id} ->
              NewUser = State#client_session.user#user{id = Id},
              NewState = State#client_session{user = NewUser},
              schat_server:bind_session(NewUser),
              StateName = active,
              State#client_session.user#user.session ! {deliver, schat_codec:packet(Id,"auth",schat_codec:enc_reg_resp(Id,"success",""))};
            _ ->
              State#client_session.user#user.session ! {deliver, schat_codec:packet("0","auth",schat_codec:enc_reg_resp_err("username or password error"))},
              NewState = State,
              StateName = wait_auth_or_register,
              io:format("auth failed")
          end;
        _ ->
          NewState = State,
          StateName = wait_auth_or_register
      end;
    _ ->
      NewState = State,
      StateName = wait_auth_or_register
  end,
  {next_state, StateName, NewState}.
active(_Event, State) ->
  case _Event of
    {packet, Packet} ->
      io:format("receive packet ~p~n", [Packet]),
      schat_route:route(Packet),
      ok;
    _ ->
      ok
  end,
  {next_state, active, State}.
stop(_Event, State) ->
  case _Event of
    _ ->
      ok
  end,
  {next_state, stop, State}.
%%----------------------------
%% private func


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_event(Event :: term(), StateName :: atom(),
    StateData :: #client_session{}) ->
  {next_state, NextStateName :: atom(), NewStateData :: #client_session{}} |
  {next_state, NextStateName :: atom(), NewStateData :: #client_session{},
    timeout() | hibernate} |
  {stop, Reason :: term(), NewStateData :: #client_session{}}).
handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_sync_event(Event :: term(), From :: {pid(), Tag :: term()},
    StateName :: atom(), StateData :: term()) ->
  {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term()} |
  {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term(),
    timeout() | hibernate} |
  {next_state, NextStateName :: atom(), NewStateData :: term()} |
  {next_state, NextStateName :: atom(), NewStateData :: term(),
    timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewStateData :: term()} |
  {stop, Reason :: term(), NewStateData :: term()}).
handle_sync_event(_Event, _From, StateName, State) ->
  Reply = ok,
  {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: term(), StateName :: atom(),
    StateData :: term()) ->
  {next_state, NextStateName :: atom(), NewStateData :: term()} |
  {next_state, NextStateName :: atom(), NewStateData :: term(),
    timeout() | hibernate} |
  {stop, Reason :: normal | term(), NewStateData :: term()}).

handle_info({deliver, Packet}, StateName, State) ->
  io:format("client_session dwmsg recived ~p~n", [Packet]),
  case gen_tcp:send(State#client_session.user#user.socket, Packet) of
    ok ->
      io:format("client_session dwmsg sended ~n");
    {error, Reason} ->
      io:format("client_session dwmsg sended error ~p ~n", Reason)
  end,
  {next_state, StateName, State};
%%handle message recived from client
%handle_info(Message,State) when is_binary(Message)->
handle_info({bind, Socket}, StateName, State) ->
  io:format("client_session bind socket ~n"),
  NewState = State#client_session{user = #user{socket = Socket, session = self()}},
  io:format("NewState ~p~n", [NewState]),
  {next_state, StateName, NewState};
%to start reciving
%% handle_info({start, Pid}, StateName, State) ->
%%   io:format("client_session:reciving start...~p~n", [State#user.socket]),
%%   NewState = State#client_session{user = #user{pid = Pid}},
%%   process_msg(NewState),
%%   {next_state, StateName, State};
handle_info({tcp, Socket, Data}, StateName, State) ->
  io:format("client_session tcp data recived ~p~n", [Data]),
  Packet = schat_parse:parse_packet(Data),
  gen_fsm:send_event(?SERVER, {packet, Packet}),
  {next_state, StateName, State};
handle_info({tcp_closed, Socket}, StateName, State) ->
  case StateName of
    active ->
      schat_server:logout(State#client_session.user);
    _ ->
      ok
  end,
  {next_state, stop, State};
handle_info(stop, StateName, State) ->
  io:format("client stop"),
  {stop, normal, State};
handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: normal | shutdown | {shutdown, term()}
| term(), StateName :: atom(), StateData :: term()) -> term()).
terminate(_Reason, _StateName, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, StateName :: atom(),
    StateData :: #client_session{}, Extra :: term()) ->
  {ok, NextStateName :: atom(), NewStateData :: #client_session{}}).
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================