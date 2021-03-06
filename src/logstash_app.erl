-module(logstash_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    Result = logstash_sup:start_link(),
    case lists:member(logstash_event_handler,
                      gen_event:which_handlers(error_logger)) of
        true ->
            pass;
        false ->
            ok = error_logger:add_report_handler(logstash_event_handler)
    end,

    %% Disable default error logger handlers and SASL handlers.
    {ok, Disable} = application:get_env(
                      logstash, disable_standard_event_handlers),
    case Disable of
        true ->
            lists:foreach(
              fun(Handler) ->
                      gen_event:delete_handler(
                        error_logger, Handler, {stop_please, ?MODULE})
              end,
              [error_logger, error_logger_tty_h, sasl_report_tty_h,
               sasl_report_file_h]);
        false ->
            pass
    end,

    Result.


stop(_State) ->
    ok.
