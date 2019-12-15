%%%-------------------------------------------------------------------
%% @doc erleans_postgres_provider public API
%% @end
%%%-------------------------------------------------------------------

-module(erleans_postgres_provider_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    erleans_postgres_provider_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
