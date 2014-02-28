%%%-------------------------------------------------------------------
%%% @author Joel Denke, Mats Maatson
%%% @coppyright (C) 2014, KTH
%%% @doc
%%%    Phone Controller supervisor
%%% @end
%%% Created : 18. feb 2014 17:40
%%%-------------------------------------------------------------------
-module(phone_fsm_sup).
-behavior(supervisor).

%% API
-export([start_link/0, add_controller/2, remove_controller/2, stop/1, init/1]).

%% Start a phone fsm supervisor
start_link() ->
  %process_flag(trap_exit, true), % Trap exits
  Link = supervisor:start_link({local, ?MODULE}, ?MODULE, []),

  case Link of
    {ok, Pid} -> {ok, Pid};
    {error, {_, Pid}} -> {ok, Pid}
  end.

%% Create a child spec
child(Module, PhoneNumber) ->
  {PhoneNumber, {Module, start_link, [PhoneNumber]},
    transient, 2000, worker, [Module]}.

init(_) ->
  {ok, {{one_for_one, 1, 1}, []}}.

%% Add child phone fsm
add_controller(SupPid, PhoneNumber) ->
  Child = supervisor:start_child(SupPid, child(phone_fsm, PhoneNumber)),
  case Child of
    {ok, Pid} -> {ok, Pid};
    {error, {_, Pid}} -> {ok, Pid}
  end.

%% Remove child phone fsm
remove_controller(SupPid, PhoneNumber) ->
  %Pid = hlr:lookup_id(PhoneNumber),
  supervisor:terminate_child(SupPid, PhoneNumber),
  supervisor:delete_child(SupPid, PhoneNumber),
  ok.

%% Stop this supervisor
stop(SupPid) ->
  exit(SupPid, normal),
  ok.
