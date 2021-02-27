-module(pulse_sup).

%% The entire purpose of the supervisor tree is to own the ETS table
%% used in pulse_db. As such, we're making the worker process as dumb
%% and simple as possible.

-export([start_link/0, start_worker/0]).

-behaviour(supervisor).

-export([init/1]).

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link(?MODULE, []).

-spec init(any()) -> {ok, {supervisor:sup_flags(), supervisor:child_spec()}}.
init(_) ->
    %% GRADUALIZER: false positive on #{} != supervisor:sup_flags()
    {ok, {#{}, [#{id => ?MODULE, start => {?MODULE, start_worker, []}}]}}.

-spec start_worker() -> {ok, pid()}.
start_worker() ->
    {ok, spawn_link(fun worker/0)}.

-spec worker() -> ok.
worker() ->
    ok = pulse_db:new(),
    receive
        _ -> ok
    end.
