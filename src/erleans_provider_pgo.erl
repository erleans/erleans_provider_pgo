%%%----------------------------------------------------------------------------
%%% Copyright Tristan Sloughter 2019. All Rights Reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% @doc
%%% @end
%%% ---------------------------------------------------------------------------
-module(erleans_provider_pgo).

%% -behaviour(erleans_provider).

-export([start_link/2,
         all/2,
         delete/3,
         read/3,
         read_by_hash/3,
         insert/5,
         insert/6,
         update/6,
         update/7]).

-include_lib("kernel/include/logger.hrl").

start_link(Name, Opts) ->
    {ok, Sup} = erleans_provider_pgo_simple_sup:start_child(Name, Opts),
    load_queries(?MODULE, filename:join(code:priv_dir(erleans_provider_pgo), "blob_provider.sql")),
    case do(Name, fun() -> create_grains_table() end, 10) of
        {error, no_db_connection} ->
            error(no_db_connection);
        _ ->
            {ok, Sup}
    end.

all(Type, ProviderName) ->
    do(ProviderName, fun() -> all_(Type) end).

read(Type, ProviderName, Id) ->
    do(ProviderName, fun() ->
                             case read_(Id, Type, erlang:phash2({Id, Type})) of
                                 {value, {_, _, ETag, State}} ->
                                     {ok, binary_to_term(State), ETag};
                                 false ->
                                     not_found
                             end
                     end).

delete(Type, ProviderName, Id) ->
    do(ProviderName,
       fun() ->
               delete_(Id, Type, erlang:phash2({Id, Type}))
       end).

read_by_hash(Type, ProviderName, Hash) ->
    do(ProviderName, fun() -> read_by_hash_(Hash, Type) end).

insert(Type, ProviderName, Id, State, ETag) ->
    insert(Type, ProviderName, Id, erlang:phash2({Id, Type}), State, ETag).

insert(Type, ProviderName, Id, Hash, State, ETag) ->
    do(ProviderName, fun() -> insert_(Id, Type, Hash, ETag, State) end).

update(Type, ProviderName, Id, State, OldETag, NewETag) ->
    update(Type, ProviderName, Id, erlang:phash2({Id, Type}), State, OldETag, NewETag).

update(Type, ProviderName, Id, Hash, State, OldETag, NewETag) ->
    do(ProviderName, fun() -> update_(Id, Type, Hash, OldETag, NewETag, State) end).

%%%

do(Pool, Fun) ->
    do(Pool, Fun, 1).

do(_Pool, _Fun, 0) ->
    ?LOG_ERROR("failed to obtain database connection"),
    {error, no_db_connection};
do(Pool, Fun, Retry) ->
    try
        pgo:transaction(Pool, Fun, #{})
    catch
        _:_ ->
            timer:sleep(200),
            do(Pool, Fun, Retry-1)
    end.

create_grains_table() ->
    #{command := create} = pgo:query(query(create_table)),
    pgo:query(query(create_idx)).

all_(Type) ->
    Q = query(select_all),
    TypeBin = atom_to_binary(Type, utf8),
    #{command := select, rows := Rows} = pgo:query(Q, [atom_to_binary(Type, utf8)]),
    {ok, [{binary_to_term(IdBin), TypeBin, ETag, binary_to_term(StateBin)}
          || {IdBin, ETag, StateBin} <- Rows]}.


read_(Id, Type, RefHash) ->
    Q = query(select),
    RefHash = erlang:phash2({Id, Type}),
    #{command := select, rows := Rows} = pgo:query(Q, [RefHash, atom_to_binary(Type, unicode)]),
    IdBin = term_to_binary(Id),
    TypeBin = atom_to_binary(Type, utf8),
    lists:search(fun({RowId, RowType, _, _}) when IdBin =:= RowId
                                                  , TypeBin =:= RowType ->
                         true;
                    (_) ->
                         false
                 end, Rows).

delete_(Id, Type, RefHash) ->
    Q = query(delete),
    RefHash = erlang:phash2({Id, Type}),
    case pgo:query(Q, [RefHash,
                       term_to_binary(Id),
                       atom_to_binary(Type, unicode)]) of
        #{command := delete} ->
            ok
    end.

read_by_hash_(Hash, Type) ->
    Q = query(select),
    #{command := select, rows := Rows} = pgo:query(Q, [Hash, atom_to_binary(Type, unicode)]),
    {ok, [{binary_to_term(IdBin), Type, ETag, binary_to_term(StateBin)}
          || {IdBin, _, ETag, StateBin} <- Rows]}.

insert_(Id, Type, RefHash, GrainETag, GrainState) ->
    Q = query(insert),
    IdBin = term_to_binary(Id),
    #{command := insert} = pgo:query(Q, [IdBin, atom_to_binary(Type, utf8), RefHash,
                                         GrainETag, term_to_binary(GrainState)]).

update_(Id, Type, RefHash, OldGrainETag, NewGrainETag, GrainState) ->
    Q = query(update),
    IdBin = term_to_binary(Id),
    case pgo:query(Q, [NewGrainETag, term_to_binary(GrainState), RefHash, IdBin,
                       atom_to_binary(Type, utf8), OldGrainETag]) of
        #{command := update, num_rows := 1} ->
            ok;
        #{command := update, num_rows := 0} ->
            {error, bad_etag}
    end.

load_queries(TabName, File) ->
    case ets:info(TabName) of
        undefined ->
            ets:new(TabName, [named_table, set, {read_concurrency, true}, protected]),
            {ok, Queries} = eql:compile(File),
            ets:insert(TabName, Queries);
        _ ->
            ok
    end.

query(Name) ->
    case ets:lookup(?MODULE, Name) of
        [] ->
            not_found;
        [{_, Query}] ->
            Query
    end.
