%%%-------------------------------------------------------------------
%%% @author snow
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 十二月 2014 上午11:40
%%%-------------------------------------------------------------------
-module(schat_listener).
-author("snow").

-define(TCP_OPTIONS, [binary, {packet, 0}, {reuseaddr, true}, {active, true}, {nodelay, true}, {keepalive, true}]).

%%
%% Exported Functions
%%
-export([start/1, accept_loop/1]).

%%
%% API Functions
%%

%%start listen server
start(Port) ->
  case (do_init(Port)) of
    {ok, ListenSocket} ->
     accept_loop(ListenSocket);
    _Els ->
      error
  end.

%%listen port
do_init(Port) when is_list(Port) ->
  start(list_to_integer(Port))
;
do_init([Port]) when is_atom(Port) ->
  start(list_to_integer(atom_to_list(Port)))
;
do_init(Port) when is_integer(Port) ->
  case gen_tcp:listen(Port, ?TCP_OPTIONS) of
    {ok, ListenSocket} ->
      {ok, ListenSocket};
    {error, Reason} ->
      {error, Reason}
  end.

%%accept client connection
accept_loop(ListenSocket) ->
  case (gen_tcp:accept(ListenSocket, 3000)) of
    {ok, Socket} ->
      process_clientSocket(Socket),
      ?MODULE:accept_loop(ListenSocket);
    {error, Reason} ->
      ?MODULE:accept_loop(ListenSocket);
    {exit, Reason} ->
      ?MODULE:accept_loop(ListenSocket)
  end.

%%process client socket
%%we should start new thread to handle client
%%generate new id using id_generator
process_clientSocket(Socket) ->
  Session = schat_server:create_session(),
  case gen_tcp:controlling_process(Socket, Session) of
    {error, Reason} ->
      io:format("bind socket error", []);
    ok ->
      Session ! {bind, Socket}
  end,
  ok.
%%
%% Local Functions
%%
