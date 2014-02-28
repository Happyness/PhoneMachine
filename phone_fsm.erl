%%%-------------------------------------------------------------------
%%% @author Joel Denke, Mats Maatson
%%% @copyright (C) 2014, KTH
%%% @doc
%%%    Phone Finite State Machine, task 3.2
%%% @end
%%% Created : 10. feb 2014 07:30
%%%-------------------------------------------------------------------
-module(phone_fsm).
-behaviour(gen_fsm).

%% gen_fsm, callback API
-export([start_link/1, init/1, stop/1, terminate/3, handle_event/3]).

% Phone FSM callback API
-export([connect/1, disconnect/1, action/2, busy/1, reject/1, accept/1, hangup/1, inbound/1]).

% States API
-export([idle/2, calling/2, receiving/2, connected/2]).

%% Miscellenaous functions ...
-export([handle_info/3, code_change/4, handle_sync_event/4, invalid_action/4]).

-define(TIMEOUT, 10000).
-define(DEBUG, false).
-record(s, {phone, fsm}).

%% Start a phone controller FSM
start_link(PhoneNumber) ->
  Link = case ?DEBUG of
    true ->  gen_fsm:start_link(?MODULE, PhoneNumber, [{debug, [trace]}]);
    false -> gen_fsm:start_link(?MODULE, PhoneNumber, [])
  end,
  case Link of
    {ok, Pid} -> {ok, Pid};%gen_server:call(?PIDNAME, {action, {init, self()}});
    {error, {_, Pid}} -> {ok, Pid}
  end.

init(PhoneNumber) ->
  process_flag(trap_exit, true),
  hlr:attach(PhoneNumber), % Attach controller to HLR
  {ok, idle, #s{phone=none, fsm=none}}.

%% Stop a phone controller FSM
stop(FsmPid) ->
  gen_fsm:send_all_state_event(FsmPid, stop).

%% Phone actions
connect(FsmPid) ->
  %io:format("Action: connect phone ~p ~n", [FsmPid]),
  gen_fsm:sync_send_all_state_event(FsmPid, connect), % Send synchronous connect message
  ok.

disconnect(FsmPid) ->
  gen_fsm:sync_send_all_state_event(FsmPid, disconnect). % Send synchronous disconnect message

% Send action to controller
action(FsmPid, Action) ->
  if ?DEBUG == true -> io:format("Action: ~p ~n", [Action]);
  true -> ok end,

  case Action of
    {outbound, PhoneNumber} ->
      gen_fsm:send_event(FsmPid, {action, {outbound, PhoneNumber}}); % Init phone call
    reject ->
      gen_fsm:send_event(FsmPid, {action, reject}); % Reject incoming phone call
    accept ->
      gen_fsm:send_event(FsmPid, {action, accept}); % Accept incoming phone call
    hangup ->
      gen_fsm:send_event(FsmPid, {action, hangup}); % Hangup current phone call
    other ->
      if ?DEBUG == true -> io:format("Action: Not implemented ~p ~n", [Action]);
       true -> ok end
  end.

%% Calls to send events between phone controllers
inbound(FsmPid) ->
  gen_fsm:send_event(FsmPid, {action, {inbound, self()}}). % Send inbound message with my own pid

busy(FsmPid) ->
  gen_fsm:send_event(FsmPid, {action, busy}).

accept(FsmPid) ->
  gen_fsm:send_event(FsmPid, {action, accept}).

hangup(FsmPid) ->
  gen_fsm:send_event(FsmPid, {action, {hangup, self()}}).

reject(FsmPid) ->
  gen_fsm:send_event(FsmPid, {action, reject}).

% Event triggered on stop
handle_event(stop, _StateName, StateData) ->
  {stop, normal, StateData}.

% Handle synchronous event actions
handle_sync_event(Action, {Pid, _}, _CurrentStateName, State) ->
  case Action of
    connect         -> {reply, ok, idle, State#s{phone=Pid}};
    disconnect      -> {reply, ok, idle, State#s{phone=none}}
  end.

% Default invalid reply
invalid_action(Phone, NextState, State, Message) ->
  if ?DEBUG == true -> io:format("Invalid: ~s \n", [Message]);
  true -> ok end,

  phone:reply(Phone, invalid),
  {next_state, NextState, State}.

%% idle state
idle({action, Action}, #s{phone=Phone} = State) ->
  case Action of
    reject -> invalid_action(Phone, idle, State, "Got reject in idle");
    accept -> invalid_action(Phone, idle, State, "Got accept in idle");
    hangup -> invalid_action(Phone, idle, State, "Got hangup in idle");
    {hangup, _Pid} -> invalid_action(Phone, idle, State, "Got hangup in idle");
    busy   -> invalid_action(Phone, idle, State, "Got busy in idle");

    {outbound, Number} ->
      case hlr:lookup_id(Number) of
        {ok, ToPid} ->
          inbound(ToPid),
          {next_state, calling, State#s{fsm=ToPid}, ?TIMEOUT}; % Set a timeout on the call
        {error, invalid} ->  % Mobile with phonenumber does not exist in HLR
          phone:reply(Phone, invalid),
          {next_state, idle, State}
      end;
    {inbound, Pid} ->
      case hlr:lookup_phone(Pid) of
        {ok, FromNumber} ->
          phone:reply(Phone, {inbound, FromNumber}),
          {next_state, receiving, State#s{fsm=Pid}};
        {error, invalid} -> % Phone number with mobile Pid does not exist in HLR
          phone:reply(Phone, invalid),
          {next_state, idle, State}
      end;

    _Wrong ->
      if ?DEBUG == true -> io:format("(Pid: ~p) Action: Not implemented ~p for state idle ~n", [Phone, Action]);
        true -> ok end,
      {next_state, idle, State}
  end.

calling(timeout, #s{phone=Phone} = State) ->
  %io:format("No answer in ~f seconds ~n", [?TIMEOUT/1000]),
  phone:reply(Phone, hangup), % Hangup on timeout
  {next_state, idle, State};

%% calling state
calling({action, Action},  #s{phone=Phone, fsm=Fsm} = State) ->
  case Action of
    reject ->
      phone:reply(Phone, reject),
      {next_state, idle, State};
    busy ->
      phone:reply(Phone, busy),
      {next_state, idle, State};
    {inbound, Pid} ->
      busy(Pid),
      phone:reply(Phone, busy),
      {next_state, calling, State};
    {outbound, _} ->
      invalid_action(Phone, calling, State, "Got outbound in calling"); % Cannot call while calling
    {hangup, _Pid} ->
      phone:reply(Phone, hangup),
      {next_state, idle, State};
    hangup ->
      hangup(Fsm),
      {next_state, idle, State};
    accept ->
      phone:reply(Phone, accept),
      {next_state, connected, State};
    _Wrong ->
      if ?DEBUG == true -> io:format("Action: Not implemented ~p for state calling ~n", [Action]);
        true -> ok end,
      {next_state, calling, State}
  end.

%% receiving state
receiving({action, Action}, #s{fsm=Fsm, phone=Phone} = State) ->
  case Action of
    hangup -> invalid_action(Phone, idle, State, "Got hangup in receiving");
    {hangup, _Pid} ->
      phone:reply(Phone, hangup),
      {next_state, idle, State};
    busy   -> invalid_action(Phone, receiving, State, "Got busy in receiving");
    {inbound, Pid} ->
      busy(Pid),
      phone:reply(Phone, busy),
      {next_state, receiving, State};

    {outbound, _} ->
      invalid_action(Phone, receiving, State, "Got outbound in receiving"); % Cannot call while receiving another call ...

    reject ->
      reject(Fsm),
      {next_state, idle, State};

    accept ->
      accept(Fsm),
      {next_state, connected, State};

    _Wrong ->
      if ?DEBUG == true -> io:format("Action: Not implemented ~p for state receiving ~n", [Action]);
        true -> ok end,
      {next_state, receiving, State}
  end.

%% connected state
connected({action, Action}, #s{fsm=Fsm, phone=Phone} = State) ->
  %io:format("State: connected ~n"),
  case Action of
    {outbound, _} -> invalid_action(Phone, connected, State, "Got outbound in connected"); % Cannot call while in another call ...
    accept ->
      {next_state, connected, State};
    {inbound, Pid} ->
      busy(Pid),
      {next_state, connected, State};
    hangup ->
      hangup(Fsm),
      {next_state, idle, State};
    {hangup, _Pid} ->
      phone:reply(Phone, hangup),
      {next_state, idle, State};
    reject -> invalid_action(Phone, connected, State, "Got reject in connected");
    busy   -> invalid_action(Phone, connected, State, "Got busy in connected");
    _Wrong ->
      if ?DEBUG == true -> io:format("Action: Not implemented ~p for state connected ~n", [Action]);
        true -> ok end,
      {next_state, connected, []}
  end.

terminate(_Reason, _StateName, _StateData) ->
  hlr:detach(). % Detach FSM from HLR before terminate process


% Unused callback API functions
code_change(_OldVsn, StateName, StateData, _Extra) ->
 % ..code to convert state (and more) during code change
{ok, StateName, StateData}.

handle_info({'EXIT', _Pid, _Reason}, StateName, StateData) ->
  %..code to handle exits here..
  {next_state, StateName, StateData}.