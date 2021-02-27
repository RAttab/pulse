-module(pulse).

-export([
    tag/2,
    tags/0, tags/1, tags/2,
    gauge/2, gauge/3,
    count/2, count/3,
    dist/2, dist/3,
    collect/1
]).

%% -type key() :: atom().
%% -type tags() :: binary().

%% -type gauge() :: undefined | {gauge, key(), tags(), number()}.
%% -type counter() :: undefined | {count, key(), tags(), integer()}.
%% -type dist() :: undefined | {dist, key(), tags(), number()}.

tag(Key, Value) when is_binary(Key) andalso is_binary(Value) ->
    <<$", Key/binary, $", $=, $", Value/binary, $">>.

tags() -> <<>>.

tags(KVs) -> tags(KVs, <<>>).

tags([], Tags) ->
    Tags;
tags([{Key, Value} | Rest], <<>>) ->
    tags(Rest, tag(Key, Value));
tags([{Key, Value} | Rest], Tags) ->
    tags(Rest, <<Tags/binary, $,, (tag(Key, Value))/binary>>).

gauge(Key, Value) -> gauge(Key, tags(), Value).

gauge(Key, Tags, Value) -> {gauge, Key, Tags, Value}.

count(Key, Value) -> count(Key, tags(), Value).

count(Key, Tags, Value) -> {count, Key, Tags, Value}.

dist(Key, Value) -> dist(Key, tags(), Value).

dist(Key, Tags, Value) -> {dist, Key, Tags, Value}.

collect(_Acc) ->
    ok.
