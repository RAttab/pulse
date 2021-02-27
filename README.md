# Pulse

Simple metrics gathering library with the following design constaints:

- ~20k requests per second processing in parallel, each emiting up to 500
  different metrics: high-throughput and highly-concurrent workload.

- Avoid declaration of metrics: declaring metrics prior to usage can be
  difficult to do in existing code basis not designed for it and also increases
  the error surface by requiring multiple locations to be whenever adding or
  modifying a metric.

- Avoid sampling: sampling tends to not adapt well with variance in workloads
  and solutions such as adapative sampling add a lot of complexity.

- Prometheus as the consumer for the metrics: to keep things simple we only
  target a single consumer as that is the only requirement.

- Support gauges, counters and distribution metrics.

- Support for tabular metrics: a metric may include a additional dimension for
  regrouping data. An arbitrary number of dimension would add a significant
  amount of complexity and was determined to not be necessary for operational
  metrics. Analytical metrics are best aggregated using a different data
  collection mechanism.


## Build

Standard `rebar3` project that includes support for:

- [erlfmt](https://github.com/WhatsApp/erlfmt)
- [gradualizer](https://github.com/josefs/Gradualizer)

As a convenience a `./build.sh` script is provided to format, type-check, run
the tests and run the benchmarks.


## Usage

```erlang
1> application:start(pulse).
ok

%% Change the value of a gauge
2> pulse:gauge(my_gauge, 12).
ok

%% Update a counter
3> [pulse:count(my_count, 1) || _ <- lists:seq(0, 100)].
[ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,
 ok,ok,ok,ok,ok,ok,ok,ok,ok,ok|...]

%% Summarize the values along multiple dimensions
4> [pulse:summarize(my_summary, {my_tag, rand:uniform(2)}, rand:uniform(100)) || _ <- lists:seq(0, 10000)].
[ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,
 ok,ok,ok,ok,ok,ok,ok,ok,ok,ok|...]

%% Export the values to prometheus
5> io:format("~s", [pulse:dump(<<"bob_the">>)]).
bob_the_my_summary{my_tag="2",quantile="0.5"} 50.922
bob_the_my_summary{my_tag="2",quantile="0.9"} 88.23599999999999
bob_the_my_summary{my_tag="2",quantile="0.99"} 96.714
bob_the_my_summary{my_tag="1",quantile="0.5"} 39.94
bob_the_my_summary{my_tag="1",quantile="0.9"} 64.44
bob_the_my_summary{my_tag="1",quantile="0.99"} 70.53999999999999
bob_the_my_gauge{} 12
bob_the_my_count{} 101
ok

6> application:stop(pulse).
ok
```


## Design

At its core, all metrics are implemented with erlang's `counters` module which
are optimized for concurrency workload and provide an array of atomic
`uint64_t`.

To support our performance criterias, quantiles are also implemented on erlang's
`counters` using a single `uint64_t` per quantile being mesured. This is
achieved using a [streaming quantile algorithm](perdu.com) that uses a
randomized control system to approximate the value of each quantile. Given our
throughput requirements, we expect the control system to have sufficient data to
generate decent approximations in a reasonable amount of time.

To avoid declaration of metrics, we use an ETS table to store the references to
our counters and metrics are added to the table when they're first
accessed. Given that erlang counters represent mutable memory, subsequent
accesses to a key will not generate any additional writes to the table. We
therefore can expect the table to stabilize in size fairly quickly and that
writes will be infrequent which results in an ETS table optimized for
`read_concurency`.

Tabular metrics are supported through a tag mechanism where a single `{TagKey,
TagVal}` tuple can be provided. Note `TagKey` must not change in between
calls. When present, the metric recorded in the table will instead become a map
with the association of tag value to metric. New values for the tag will cause
additional writes into the ETS table but we expect the cardinality of each tag
to be bounded which preserve our assumption that the ETS table will eventually
stabilize in size and that writes will be infrequent. Tags with large
cardinality should be relegated to analytical data-flow.

Metric keys and tag keys are both provided as atoms due to their dual
representation where they can be both an integer and an string. The integer
representation is the default internal representation which is used for keying
the ETS table and the maps which benefit from having a small consistent integer
for their lookup. The string representation is used when exporting the metrics
to the prometheus format which while costly, is done infrequently and the
performance hit is acceptable.

Pulse is provided as application instead of a library to provide a persistent
owner of the ETS table. When the application is stopped all metrics will be
forgotten as the table will be deleted.


## Perf

TODO
