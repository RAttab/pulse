-module(db_bench).

-include_lib("eunit/include/eunit.hrl").

-define(RUNS, 1000).

write(Type, N, P) ->
    pulse_db:new(),
    try
        [pulse_db:write(Type, rand_key(Type), 1) || _ <- lists:seq(0, N)],
        Fn = fun() -> [pulse_db:write(Type, key, 1) || _ <- lists:seq(0, 100)] end,
        Fn(),
        report(#{name => write, type => Type, size => N, procs => P}, timing:function(Fn, ?RUNS, P))
    after
        pulse_db:free()
    end.

write_test() ->
    [
        write(Type, N, P)
        || Type <- [gauge, count, summary], N <- [1, 10, 100, 200, 500], P <- [1, 2, 4, 8]
    ].

write_tags(Type, N, P) ->
    pulse_db:new(),
    try
        [pulse_db:write(Type, rand_key(Type), rand_tag(tag, N), 1) || _ <- lists:seq(0, 100 * N)],
        Tag = rand_tag(tag, N),
        Fn = fun() -> [pulse_db:write(Type, key, Tag, 1) || _ <- lists:seq(0, 100)] end,
        Fn(),
        report(#{name => tags, type => Type, size => N, procs => P}, timing:function(Fn, ?RUNS, P))
    after
        pulse_db:free()
    end.

write_tags_test() ->
    [
        write_tags(Type, N, P)
        || Type <- [gauge, count, summary], N <- [1, 10, 100], P <- [1, 2, 4, 8]
    ].

rand_key(Type) ->
    rand_atom(Type, 100000).

rand_tag(Key, N) ->
    {Key, rand_atom(val, N)}.

rand_atom(Name, N) ->
    Suffix = erlang:integer_to_list(rand:uniform(N)),
    Prefix = erlang:atom_to_list(Name),
    erlang:list_to_atom(lists:flatten([Prefix, "_", Suffix])).

report(Id, Result) ->
    erlang:display({Id, lists:keyfind(percentile, 1, Result)}).
