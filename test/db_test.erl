-module(db_test).

-include_lib("eunit/include/eunit.hrl").

dump_test() ->
    pulse_db:new(),
    try
        pulse_db:write(gauge, gauge, 100),
        pulse_db:write(gauge, gauge_tag, {dim, a}, 100),
        pulse_db:write(gauge, gauge_tag, {dim, b}, 200),
        pulse_db:write(gauge, gauge_tag, {dim, c}, 300),

        pulse_db:write(count, count, 100),
        pulse_db:write(count, count, -200),
        pulse_db:write(count, count_tag, {dim, a}, 100),
        pulse_db:write(count, count_tag, {dim, b}, 200),
        pulse_db:write(count, count_tag, {dim, c}, 300),

        Seq = lists:seq(0, 10000),
        [pulse_db:write(summary, summary, rand:uniform(100)) || _ <- Seq],
        [pulse_db:write(summary, summary_tag, {dim, a}, rand:uniform(100)) || _ <- Seq],
        [pulse_db:write(summary, summary_tag, {dim, b}, rand:uniform(100)) || _ <- Seq],
        [pulse_db:write(summary, summary_tag, {dim, c}, rand:uniform(100)) || _ <- Seq],

        io:format(standard_error, "~n----------------------~n~s~n----------------------~n~n", [
            iolist_to_binary(pulse_db:dump(<<"bob_the">>))
        ])
    after
        pulse_db:free()
    end.
