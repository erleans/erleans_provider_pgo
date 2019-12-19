%%%-------------------------------------------------------------------
%% @doc erleans_provider_pgo top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erleans_provider_pgo_simple_sup).

-behaviour(supervisor).

-export([start_link/0,
         start_child/2,
         init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(ProviderName, PoolOpts) ->
    supervisor:start_child(?MODULE, [ProviderName, PoolOpts]).

init(_Args) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 5,
                 period => 10},
    ChildSpecs = [#{id => erleans_provider_pgo_sup,
                    start => {erleans_provider_pgo_sup, start_link, []},
                    type => supervisor,
                    shutdown => 3000}],
    {ok, {SupFlags, ChildSpecs}}.
