%%%-------------------------------------------------------------------
%% @doc erleans_provider_pgo public API
%% @end
%%%-------------------------------------------------------------------

-module(erleans_provider_pgo_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    erleans_provider_pgo_simple_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
