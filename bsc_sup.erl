%%%-------------------------------------------------------------------
%%% @author Joel Denke, Mats Maatson
%%% @coppyright (C) 2014, KTH
%%% @doc
%%%    Phone Controller supervisor
%%% @end
%%% Created : 18. feb 2014 17:40
%%%-------------------------------------------------------------------
-module(bsc_sup).
-behavior(supervisor).

%% API
-export([start_link/0, add_controller/1, remove_controller/1, init/1, child/1]).

%% Own functions
-export([get_fsm_pid/0]).

%% Start a bsc supervisor
start_link() ->
  Link = supervisor:start_link({local, ?MODULE}, ?MODULE, []),

  case Link of
    {ok, Pid} -> {ok, Pid};
    {error, {_, Pid}} -> {ok, Pid}
  end.

%% Initiates supervisor
init(_) ->
  Specs = [child(hlr), child(fsm_sup)],
  ok = supervisor:check_childspecs(Specs),
  {ok, {{one_for_one, 1, 1}, Specs}}.

%% Add child phone fsm supervisor
add_controller(PhoneNumber) ->
  Pid = get_fsm_pid(),

  case Pid of
    undefined -> {error, "FSM supervisor does not exist"};
    Result    ->  phone_fsm_sup:add_controller(Result, PhoneNumber)
  end.

%% Create a child spec
child(Module) ->
  case Module of
    hlr ->
      {Module, {Module, start_link, []}, permanent, 2000, worker, [Module]};
    fsm_sup ->
      {Module, {phone_fsm_sup, start_link, []}, permanent, 2000, supervisor, [fsm_sup, gen_server]}
  end.

%% Remove child phone fsm supervisor
remove_controller(PhoneNumber) ->
  Pid = get_fsm_pid(),
  phone_fsm_sup:remove_controller(Pid, PhoneNumber),
  {ok, Pid}.

get_fsm_pid() -> whereis(phone_fsm_sup).