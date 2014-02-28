%%%-------------------------------------------------------------------
%%% @author Joel Denke, Mats Maatson
%%% @copyright (C) 2014, KTH
%%% @doc
%%%   Uppgift 8.3 (A)
%%%   DETS database table implementation with same interface as task 2.4
%%% @end
%%%-------------------------------------------------------------------
-module(db_dets).

%% API
-export([new/0, new/1, destroy/1, delete/2, write/3, read/2, match/2, update/1]).
-record(data, {key, value}).
-define(DISKID, dbDisk).
-define(DISKFILE, "dets_table").

new() ->
  new(?DISKFILE).

new(FileName) ->
  {ok, Ref} = dets:open_file(?DISKID, [{file, FileName}, {keypos, #data.key}]),
  Ref.

destroy(_Table) ->
  dets:delete_all_objects(?DISKID),
  %dets:close(?DISKID),
  ok.

% Update value in list
update(Record) ->
  dets:insert(?DISKID, Record),
  ok.

% Write new tuple to the list
write(Key, Element, Table) ->
  Record = #data{key=Key, value=Element},
  update(Record),
  Table.

% Delete Element from list with specified key
delete(Key, Table) ->
  dets:delete(?DISKID, Key),
  Table.

% Read value from list, specified by key
read(Key, Table) ->
  case dets:lookup(Table, Key) of
    [{data, _, Value}] -> {ok, Value};
    []       -> {error, instance} % Keep scanning
  end.

% Match all elements in list
match(Element, Table) ->
  Result = dets:match_object(Table, {data, '$1', Element}),
  if is_list(Result) ->
    Result;
  true -> []
  end.
