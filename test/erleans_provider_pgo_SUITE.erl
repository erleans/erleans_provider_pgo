-module(erleans_provider_pgo_SUITE).

-compile(export_all).

all() ->
    [basic_insert].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(pgo),
    {ok, _} =
        application:ensure_all_started(erleans_provider_pgo),
    {ok, _} = application:ensure_all_started(erleans),
    Config.

end_per_suite(_Config) ->
    application:stop(erleans),
    application:stop(erleans_provider_pgo),
    application:stop(pgo),
    ok.

basic_insert(_Config) ->
    Grain1 = erleans:get_grain(test_grain, <<"grain1">>),
    test_grain:activated_counter(Grain1),
    test_grain:save(Grain1),

    ok.
