-module(mochijson2_factory).

-export([new/0]).


new() ->
    fun logstash_mochijson2:encode/1.
