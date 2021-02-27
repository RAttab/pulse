-module(pulse_db).

-export([new/0, free/0, write/3, write/4, dump/1]).

-define(DB, pulse).

-define(SUMMARY_ADJUST_RATIO, 1000).

-type counters() :: counters:counters_ref().

-record(tag, {key :: atom(), values :: map()}).
-record(gauge, {value :: counters()}).
-record(count, {value :: counters()}).
-record(summary, {
    base :: number(),
    adjust :: number(),
    quantiles :: counters()
}).

-type type() :: gauge | count | summary.
-type metric() :: #tag{} | #gauge{} | #count{} | #summary{}.

-spec new() -> ok.
new() ->
    ?DB = ets:new(?DB, [set, public, named_table, {read_concurrency, true}]),
    ok.

-spec free() -> ok.
free() ->
    true = ets:delete(?DB),
    ok.

-spec write(type(), pulse:key(), number()) -> ok.
write(Type, Key, Value) ->
    write(Type, Key, {}, Value).

-spec write(type(), pulse:key(), pulse:tag(), number()) -> ok.
write(Type, Key, Tag, Value) ->
    update(Type, Key, Tag, Value, metric(Type, Key, Tag, Value)).

-spec metric(type(), pulse:key(), pulse:tag(), number()) -> metric().
metric(Type, Key, Tag, Value) ->
    case ets:lookup(?DB, Key) of
        [{Key, Metric}] ->
            Metric;
        [] ->
            Metric = new(Type, Tag, Value),
            case ets:insert_new(?DB, {Key, Metric}) of
                false ->
                    [{Key, Metric}] = ets:lookup(?DB, Key),
                    Metric;
                _ ->
                    Metric
            end
    end.

-spec new(type(), pulse:tag(), number()) -> metric().
new(Type, {TagKey, TagVal}, Value) ->
    #tag{key = TagKey, values = #{TagVal => new(Type, {}, Value)}};
new(gauge, {}, _) ->
    #gauge{value = counters:new(1, [atomics])};
new(count, {}, _) ->
    #count{value = counters:new(1, [write_concurrency])};
new(summary, {}, Value) ->
    #summary{
        base = Value,
        adjust = Value / ?SUMMARY_ADJUST_RATIO,
        quantiles = counters:new(3, [write_concurrency])
    }.

-spec update(type(), pulse:key(), pulse:tag(), number(), metric()) -> ok.
update(gauge, _Key, {}, Value, #gauge{value = Counter}) when is_integer(Value) ->
    counters:put(Counter, 1, Value);
update(count, _Key, {}, Value, #count{value = Counter}) when is_integer(Value) ->
    counters:add(Counter, 1, Value);
update(summary, _Key, {}, Value, Summary = #summary{}) ->
    update_quantile(Summary, 1, 0.50, Value),
    update_quantile(Summary, 2, 0.90, Value),
    update_quantile(Summary, 3, 0.99, Value);
update(Type, Key, {TagKey, TagVal}, Value, Tag = #tag{key = TagKey, values = Map}) ->
    case Map of
        #{TagVal := Metric} ->
            update(Type, Key, {}, Value, Metric);
        _ ->
            Metric = new(Type, {}, Value),
            update(Type, Key, {}, Value, Metric),
            true = ets:insert(?DB, {Key, Tag#tag{values = Map#{TagVal => Metric}}}),
            ok
    end;
update(Type, Key, Tag, Value, Metric) ->
    error({pulse, mismatch, {Type, Key, Tag, Value, Metric}}).

% TODO: Detect whether adjust is too coarse or fine to reset the
% counter.
-spec update_quantile(#summary{}, pos_integer(), float(), number()) -> ok.
update_quantile(#summary{base = Base, adjust = Adjust, quantiles = Counters}, Ix, Quantile, Value) ->
    Estimate = Base + Adjust * counters:get(Counters, Ix),
    case {Value < Estimate, rand:uniform() < Quantile} of
        {true, false} -> counters:sub(Counters, Ix, 1);
        {false, true} -> counters:add(Counters, Ix, 1);
        _ -> ok
    end.

-spec dump(binary()) -> iolist().
dump(Prefix) ->
    ets:foldl(fun(Row, Acc) -> encode(Prefix, Row, Acc) end, [], ?DB).

-spec encode(binary(), {pulse:key(), metric()}, iolist()) -> iolist().
encode(Prefix, {Key, #tag{key = TagKey, values = Map}}, Acc) ->
    maps:fold(
        fun(TagVal, Metric, AccInner) ->
            encode(Prefix, Key, {TagKey, TagVal}, Metric, AccInner)
        end,
        Acc,
        Map
    );
encode(Prefix, {Key, Metric}, Acc) ->
    encode(Prefix, Key, {}, Metric, Acc).

-spec encode(binary(), pulse:key(), pulse:tag(), metric(), iolist()) -> iolist().
encode(Prefix, Key, Tag, #gauge{value = Counter}, Acc) ->
    [
        encode_key(Prefix, Key),
        encode_tags([Tag]),
        $\s,
        encode_value(counters:get(Counter, 1)),
        $\n
        | Acc
    ];
encode(Prefix, Key, Tag, #count{value = Counter}, Acc) ->
    [
        encode_key(Prefix, Key),
        encode_tags([Tag]),
        $\s,
        encode_value(counters:get(Counter, 1)),
        $\n
        | Acc
    ];
encode(Prefix, Key, Tag, Summary = #summary{}, Acc) ->
    KeyStr = encode_key(Prefix, Key),
    [
        encode_quantile(KeyStr, Tag, Summary, 1, 0.5),
        encode_quantile(KeyStr, Tag, Summary, 2, 0.9),
        encode_quantile(KeyStr, Tag, Summary, 3, 0.99)
        | Acc
    ].

-spec encode_quantile(iolist(), pulse:tag(), #summary{}, pos_integer(), float()) -> iolist().
encode_quantile(
    KeyStr,
    Tag,
    #summary{base = Base, adjust = Adjust, quantiles = Counters},
    Ix,
    Quantile
) ->
    [
        KeyStr,
        encode_tags([{quantile, Quantile}, Tag]),
        $\s,
        encode_value(Base + Adjust * counters:get(Counters, Ix)),
        $\n
    ].

-spec encode_key(binary(), pulse:key()) -> iolist().
encode_key(Prefix, Key) -> [Prefix, $_, atom_to_binary(Key, utf8)].

-spec encode_tags([pulse:tag()]) -> iolist().
encode_tags(Tags) -> [${, encode_tags(Tags, []), $}].

-spec encode_tags([pulse:tag()], iolist()) -> iolist().
encode_tags([], Acc) -> Acc;
encode_tags([{} | Rest], Acc) -> encode_tags(Rest, Acc);
encode_tags([{Key, Val} | Rest], []) -> encode_tags(Rest, [encode_tag(Key, Val)]);
encode_tags([{Key, Val} | Rest], Acc) -> encode_tags(Rest, [encode_tag(Key, Val), $, | Acc]).

-spec encode_tag(pulse:tag_key(), pulse:tag_val()) -> iolist().
encode_tag(Key, Val) -> [atom_to_binary(Key, utf8), $=, $", encode_tag_value(Val), $"].

-spec encode_tag_value(pulse:tag_val()) -> iodata().
encode_tag_value(Val) when is_binary(Val) -> Val;
encode_tag_value(Val) when is_atom(Val) -> atom_to_binary(Val, utf8);
%% GRADUALIZER: false positive (kinda) on char() != byte() for iolist().
encode_tag_value(Val) when is_number(Val) -> io_lib:format("~p", [Val]).

%% GRADUALIZER: false positive (kinda) on char() != byte() for iolist().
-spec encode_value(number()) -> iolist().
encode_value(Value) -> io_lib:format("~p", [Value]).
