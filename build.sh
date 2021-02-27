#! /bin/sh

./rebar3 fmt --write
./rebar3 compile
./rebar3 gradualizer
./rebar3 eunit -v
