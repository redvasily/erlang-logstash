-module(logstash_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).


start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


init([]) ->
    Child = {logstash, {logstash, start_link, []},
             permanent, 2000, worker, [logstash]},
    {ok, {{one_for_all, 1000, 1}, [Child]}}.
