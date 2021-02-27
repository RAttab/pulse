-module(pulse_db).

-export([collect/2, report/2]).

-record(gauge, {value = 0 :: number()}).
-record(count, {value = 0 :: integer()}).
-record(dist, {
    base :: number(),
    adjust :: number(),
    p50 = 0 :: integer(),
    p90 = 0 :: integer(),
    p99 = 0 :: integer()
}).

-define(DIST_ADJUST_RATIO, 1000).

collect(Db, []) ->
    Db;
collect(Db, [Metric = {_, Key, Tags, _} | Rest]) ->
    Index = {Key, Tags},
    case Db of
        #{Index := Current} ->
            collect(Db#{Index => update(Current, Metric)}, Rest);
        _ ->
            collect(Db#{Index => update(new(Metric), Metric)}, Rest)
    end.

new({gauge, _, _, _}) -> #gauge{};
new({count, _, _, _}) -> #count{};
new({dist, _, _, Value}) -> #dist{base = Value, adjust = Value / ?DIST_ADJUST_RATIO}.

update(Gauge = #gauge{}, {gauge, _, _, Value}) ->
    Gauge#gauge{value = Value};
update(Count = #count{value = Current}, {count, _, _, Value}) ->
    Count#count{value = Current + Value};
update(
    Dist = #dist{base = Base, adjust = Adjust, p50 = P50, p90 = P90, p99 = P99},
    {dist, _, _, Value}
) ->
    %% TODO: Allow for dist reset if granularity is too coarse or too fine.
    Dist#dist{
        p50 = P50 + update_quantile(0.50, Base + Adjust * P50, Value, rand:uniform()),
        p90 = P90 + update_quantile(0.90, Base + Adjust * P90, Value, rand:uniform()),
        p99 = P99 + update_quantile(0.99, Base + Adjust * P99, Value, rand:uniform())
    }.

update_quantile(Quantile, Estimate, Value, Rand) when Value < Estimate andalso Rand > Quantile ->
    -1;
update_quantile(Quantile, Estimate, Value, Rand) when Value > Estimate andalso Rand < Quantile ->
    +1;
update_quantile(_, _, _, _) ->
    0.

report(Db, Prefix) ->
    [encode(Prefix, Key, Entry) || {Key, Entry} <- Db].

encode(Prefix, {Key, Tags}, #gauge{value = Value}) ->
    [encode_key(Prefix, Key, Tags), $\s, encode_num(Value)];
encode(Prefix, {Key, Tags}, #count{value = Value}) ->
    [encode_key(Prefix, Key, Tags), $\s, encode_num(Value)];
encode(Prefix, {Key, Tags}, #dist{base = Base, adjust = Adjust, p50 = P50, p90 = P90, p99 = P99}) ->
    MakeTags = fun(Quantile) -> pulse:tags([{<<"quantile">>, Quantile}], Tags) end,
    [
        [encode_key(Prefix, Key, MakeTags(<<"0.50">>)), $\s, encode_num(Base + Adjust * P50)],
        [encode_key(Prefix, Key, MakeTags(<<"0.90">>)), $\s, encode_num(Base + Adjust * P90)],
        [encode_key(Prefix, Key, MakeTags(<<"0.99">>)), $\s, encode_num(Base + Adjust * P99)]
    ].

encode_key(Prefix, Key, Tags) when is_atom(Key) ->
    encode_key(Prefix, atom_to_binary(Key, utf8), Tags);
encode_key(Prefix, Key, Tags) ->
    <<Prefix/binary, Key/binary, ${, Tags/binary, $}>>.

encode_num(Value) when is_float(Value) ->
    list_to_binary(float_to_list(Value));
encode_num(Value) when is_integer(Value) ->
    list_to_binary(integer_to_list(Value)).
