-module(db_bench).

-include_lib("eunit/include/eunit.hrl").

-define(RUNS, 1000).

rand_test() ->
    Fn = fun() -> rand_key(gauge) end,
    report(rand, timing:function(Fn, ?RUNS, 1)).

collect_gauge(N) ->
    Metrics = [rand_metric(gauge) || _ <- lists:seq(0, N)],
    Db = pulse_db:collect(#{}, Metrics),
    Fn = fun() -> pulse_db:collect(Db, Metrics) end,
    report({gauge, N}, timing:function(Fn, ?RUNS, 1)).

collect_gauge_test() ->
    [collect_gauge(N) || N <- [1, 10, 100, 200, 500]].

collect_dist(N) ->
    Metrics = [rand_metric(dist) || _ <- lists:seq(0, N)],
    Db = pulse_db:collect(#{}, Metrics),
    Fn = fun() -> pulse_db:collect(Db, Metrics) end,
    report({dist, N}, timing:function(Fn, ?RUNS, 1)).

collect_dist_test() ->
    [collect_dist(N) || N <- [1, 10, 100, 200, 500]].

collect_mix_test() ->
    Metrics = lists:flatten([
        [rand_metric(count) || _ <- lists:seq(0, 200)],
        [rand_metric(dist) || _ <- lists:seq(0, 50)]
    ]),
    Db = pulse_db:collect(#{}, Metrics),
    Fn = fun() -> pulse_db:collect(Db, Metrics) end,
    report({mix, 200, 50}, timing:function(Fn, ?RUNS, 1)).

rand_key(Type) ->
    Suffix = erlang:integer_to_list(rand:uniform(100000)),
    Prefix = erlang:atom_to_list(Type),
    erlang:list_to_atom(lists:flatten([Suffix, "_", Prefix])).

rand_metric(Type) ->
    {Type, rand_key(Type), pulse:tags(), rand:uniform(100)}.

report(Id, Result) ->
    Keys = [min, max, percentile],
    Map = maps:from_list(lists:filter(fun({K, _}) -> lists:member(K, Keys) end, Result)),
    erlang:display({Id, Map}).
