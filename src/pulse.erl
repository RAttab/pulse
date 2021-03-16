-module(pulse).

-export([
    gauge/2, gauge/3,
    count/2, count/3,
    summarize/2, summarize/3,
    now_us/0,
    summarize_since_us/2, summarize_since_us/3,
    dump/1
]).

-export_type([key/0, tag_key/0, tag_val/0, tag/0]).

-behaviour(application).

-export([start/2, stop/1]).

-type key() :: atom().
-type tag_key() :: atom().
-type tag_val() :: atom() | binary() | number().
-type tag() :: {} | {tag_key(), tag_val()}.

-spec gauge(key(), integer()) -> ok.
gauge(Key, Value) -> pulse_db:write(gauge, Key, Value).

-spec gauge(key(), tag(), integer()) -> ok.
gauge(Key, Tag, Value) -> pulse_db:write(gauge, Key, Tag, Value).

-spec count(key(), integer()) -> ok.
count(Key, Value) -> pulse_db:write(count, Key, Value).

-spec count(key(), tag(), integer()) -> ok.
count(Key, Tag, Value) -> pulse_db:write(count, Key, Tag, Value).

-spec summarize(key(), number()) -> ok.
summarize(Key, Value) -> pulse_db:write(summary, Key, Value).

-spec summarize(key(), tag(), number()) -> ok.
summarize(Key, Tag, Value) -> pulse_db:write(summary, Key, Tag, Value).

-spec now_us() -> erlang:timestamp().
now_us() -> os:timestamp().

-spec summarize_since_us(key(), erlang:timestamp()) -> ok.
summarize_since_us(Key, T0) ->
    summarize(Key, timer:now_diff(now_us(), T0)).

-spec summarize_since_us(key(), tag(), erlang:timestamp()) -> ok.
summarize_since_us(Key, Tag, T0) ->
    summarize(Key, Tag, timer:now_diff(now_us(), T0)).

-spec dump(binary()) -> iolist().
dump(Prefix) -> pulse_db:dump(Prefix).

-spec start(any(), any()) -> supervisor:startlink_ret().
start(_, _) -> pulse_sup:start_link().

-spec stop(any()) -> ok.
stop(_) -> ok.
