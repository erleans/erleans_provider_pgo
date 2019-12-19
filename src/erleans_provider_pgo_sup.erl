%%%-------------------------------------------------------------------
%% @doc erleans_provider_pgo top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erleans_provider_pgo_sup).

-export([start_link/2,
         init/1]).

-define(SERVER, ?MODULE).

start_link(ProviderName, PoolOpts) ->
    supervisor:start_link(?MODULE, [ProviderName, PoolOpts]).

init([ProviderName, PoolOpts]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 5,
                 period => 10},
    ChildSpecs = [#{id => {pgo_pool, ProviderName},
                    start => {pgo_pool, start_link, [ProviderName, PoolOpts]},
                    restart => permanent,
                    shutdown => 3000,
                    type => worker,
                    modules => [pgo_pool]}],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
