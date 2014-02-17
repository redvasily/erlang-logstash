-module(logstash_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").


suite() ->
    [{timetrap, {seconds, 30}}].


all() ->
    [basic, basic_list, error_logger, error_logger2].


init_per_suite(Config) ->
    Config.


end_per_suite(_Config) ->
    ok.


-spec set_env(Config :: any(),
              App :: atom(),
              Variables :: [{Var :: atom(), Value :: any()}]) ->
                     New_config :: any().
set_env(Config, App, Variables) ->
    application:load(App),
    Restore = proplists:get_value(restore_env, Config, orddict:new()),
    New_restore =
        lists:foldl(
          fun({Var, Value}, Acc) ->
                  Current_value = application:get_env(App, Var),
                  application:set_env(App, Var, Value),
                  case orddict:is_key({App, Var}, Acc) of
                      true ->
                          %% This value has been already set. Do nothing
                          Acc;
                      false ->
                          %% This value hasn't been set before.
                          %% Store current value
                          orddict:store({App, Var}, Current_value, Acc)
                  end
          end,
          Restore,
          Variables),
    lists:keystore(restore_env, 1, Config, {restore_env, New_restore}).


restore_env(Config) ->
    Restore = proplists:get_value(restore_env, Config, orddict:new()),
    orddict:fold(
      fun({App, Var}, Value, Acc_in) ->
              application:set_env(App, Var, Value),
              Acc_in
      end,
      undefined,
      Restore),
    Config.


prepare(Config) ->
    Priv_dir = ?config(priv_dir, Config),
    application:load(logstash),
    File = filename:join(Priv_dir, "test.json"),
    Config2 = set_env(Config, logstash, [{filename, File}]),
    Config2.


init_per_testcase(_TestCase, Config) ->
    Config2 = prepare(Config),
    application:start(logstash),
    Config2.


end_per_testcase(_TestCase, Config) ->
    application:stop(logstash),
    restore_env(Config),
    ok.


read_lines(Io_device) ->
    read_lines(Io_device, []).

read_lines(Io_device, Acc) ->
    case file:read_line(Io_device) of
        {ok, Line} ->
            read_lines(Io_device, [Line | Acc]);
        eof ->
            lists:reverse(Acc)
    end.


basic(_Config) ->
    logstash:send(<<"Some message">>),
    logstash:deliver(),
    timer:sleep(100),

    {ok, Filename} = application:get_env(logstash, filename),
    {ok, File} = file:open(Filename, [read, binary]),
    Lines = read_lines(File),
    file:close(File),

    Decoded = [logstash_mochijson2:decode(Line) || Line <- Lines],
    Messages = [proplists:get_value(<<"message">>, Properties) ||
                   {struct, Properties} <- Decoded],
    true = lists:member(<<"Some message">>, Messages),
    ok.


basic_list(_Config) ->
    logstash:send("Some message"),
    logstash:deliver(),
    timer:sleep(100),

    {ok, Filename} = application:get_env(logstash, filename),
    {ok, File} = file:open(Filename, [read, binary]),
    Lines = read_lines(File),
    file:close(File),

    Decoded = [logstash_mochijson2:decode(Line) || Line <- Lines],
    Messages = [proplists:get_value(<<"message">>, Properties) ||
                   {struct, Properties} <- Decoded],
    true = lists:member(<<"Some message">>, Messages),
    ok.


error_logger(_Config) ->
    error_logger:error_msg("error_logger:error_msg"),
    logstash:deliver(),
    timer:sleep(100),

    {ok, Filename} = application:get_env(logstash, filename),
    {ok, File} = file:open(Filename, [read, binary]),
    Lines = read_lines(File),
    file:close(File),

    Decoded = [logstash_mochijson2:decode(Line) || Line <- Lines],
    Messages = [proplists:get_value(<<"message">>, Properties) ||
                   {struct, Properties} <- Decoded],
    true = lists:member(<<"error_logger:error_msg">>, Messages),
    ok.


error_logger2(_Config) ->
    error_logger:error_msg("error_logger:error_msg ~p", [[1, 2]]),
    logstash:deliver(),
    timer:sleep(100),

    {ok, Filename} = application:get_env(logstash, filename),
    {ok, File} = file:open(Filename, [read, binary]),
    Lines = read_lines(File),
    file:close(File),

    Decoded = [logstash_mochijson2:decode(Line) || Line <- Lines],
    Messages = [proplists:get_value(<<"message">>, Properties) ||
                   {struct, Properties} <- Decoded],
    true = lists:member(<<"error_logger:error_msg [1,2]">>, Messages),
    ok.
