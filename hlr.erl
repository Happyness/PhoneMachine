%%%-------------------------------------------------------------------
%%% @author Joel Denke, Mats Maatson
%%% @copyright (C) 2014, KTH
%%% @doc
%%%    Home location register, task 3.1
%%% @end
%%% Created : 10. feb 2014 07:30
%%%-------------------------------------------------------------------
-module(hlr).
-behaviour(gen_server).
-define(EMPTYNUMBER, 0).
-define(EMPTYPID, 0).
-define(DBHLR, "dbHlr").
-define(PIDNAME, {global, ?MODULE}).
-define(DBMODULE, db_dets).

%% API
-export([start_link/0, attach/1, detach/0, lookup_id/1, lookup_phone/1]).
%Gen_server api functions
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
-export([stop/0]).

stop() ->
  %gen_server:call(?MODULE, {action, destroy}),
  gen_server:cast(?PIDNAME, stop).

% HLR API
start_link() ->
  case gen_server:start_link(?PIDNAME, ?MODULE, ?DBMODULE, []) of
    {ok, Pid} -> {ok, Pid};%gen_server:call(?PIDNAME, {action, {init, self()}});
    {error, {_, Pid}} -> {ok, Pid}
  end.

attach(PhoneNumber) ->
  gen_server:call(?PIDNAME, {action, {attach, PhoneNumber}}).

detach() ->
  gen_server:call(?PIDNAME, {action, detach}).

lookup_id(PhoneNumber) ->
  gen_server:call(?PIDNAME, {action, {lookup_id, PhoneNumber}}).

lookup_phone(Pid) ->
  gen_server:call(?PIDNAME, {action, {lookup_phone, Pid}}).


% ----------------------- Implement gen_server callback API -----------------------
init(DbModule) ->
  process_flag(trap_exit, true), % Trap exits
  State = {DbModule, DbModule:new(?DBHLR)},
  {ok, State}.

terminate(_, {DbModule, Db}) ->
  io:format("Terminating HLR\n"),
  DbModule:destroy(Db),
  ok.

handle_cast(stop, State) ->
  {stop, ok, State}.

handle_call({action, Action}, {CallPid, _}, {DbModule, Db}) ->
  io:format("~p~n", [CallPid]),

  case Action of
    {attach, PhoneNumber} ->
      {reply, ok, {DbModule, DbModule:write(PhoneNumber, CallPid, Db)}};
    detach ->
      case DbModule:match(CallPid, Db) of
        [{_, Key, _}]      -> {reply, ok, {DbModule, DbModule:delete(Key, Db)}};
        []                 -> {reply, ok, {DbModule, Db}}
      end;

    {lookup_id, PhoneNumber} ->
      case DbModule:read(PhoneNumber, Db) of
        {ok, Value}       -> {reply, {ok, Value}, {DbModule, Db}};
        {error, instance} -> {reply, {error, invalid}, {DbModule, Db}}
      end;

    {lookup_phone, Pid} ->
      case DbModule:match(Pid, Db) of
        [{_, Key, _}]      -> {reply, {ok, Key}, {DbModule, Db}};
        []                 -> {reply, {error, invalid}, {DbModule, Db}}
      end
  end.

handle_info(_, State) -> {noreply, State}.
code_change(_, State, _) -> {ok, State}.
