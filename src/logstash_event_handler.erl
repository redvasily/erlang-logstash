-module(logstash_event_handler).
-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2,
         code_change/3]).


-export([format/2, format/1]).


init(_) ->
    {ok, []}.


pid_info(Pid) ->
    [{pid, logstash:conv_binary(Pid)}].


handle_event({error, _Gleader, {Pid, Format, Data}}, State) when is_pid(Pid) ->
    logstash:send(error, format(Format, Data), pid_info(Pid)),
    {ok, State};

handle_event({error_report, _Gleader, {Pid, std_error, Report}}, State)
  when is_pid(Pid) ->
    logstash:send(error, format(Report), pid_info(Pid)),
    {ok, State};

handle_event({error_report, _Gleader, {Pid, Type, Report}}, State)
  when is_pid(Pid), is_atom(Type) ->
    logstash:send(Type, format(Report), pid_info(Pid)),
    {ok, State};

handle_event({warning_msg, _Gleader, {Pid, Format, Data}}, State)
  when is_pid(Pid) ->
    logstash:send(warning, format(Format, Data), pid_info(Pid)),
    {ok, State};

handle_event({warning_report, _Gleader, {Pid, std_warning, Report}}, State)
  when is_pid(Pid) ->
    logstash:send(warning, Report, pid_info(Pid)),
    {ok, State};

handle_event({warning_report, _Gleader, {Pid, Type, Report}}, State)
  when is_pid(Pid), is_atom(Type) ->
    logstash:send(Type, format(Report), pid_info(Pid)),
    {ok, State};

handle_event({info_msg, _Gleader, {Pid, Format, Data}}, State)
  when is_pid(Pid) ->
    logstash:send(info, format(Format, Data), pid_info(Pid)),
    {ok, State};

handle_event({info_report, _Gleader, {Pid, std_info, Report}}, State)
  when is_pid(Pid) ->
    logstash:send(info, format(Report), pid_info(Pid)),
    {ok, State};

handle_event({info_report, _Gleader, {Pid, Type, Report}}, State)
  when is_pid(Pid), is_atom(Type) ->
    logstash:send(Type, format(Report), pid_info(Pid)),
    {ok, State};

handle_event(_, State) ->
    {ok, State}.


handle_call(_Request, State) ->
    {ok, ok, State}.


handle_info(_Info, State) ->
    {ok, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


format(Event={event, _Severity, _Module, _Message, _Counter}) ->
    Event;

format(Data) ->
    case config:get(syslog, truncate_messages) of
        false ->
            event:format("~p", [Data]);
        true ->
            {Msg, _Size} = trunc_io:print(Data, 10240),
            Bin = force_binary(Msg),
            Bin
    end.


format(Format, Data) ->
    case Data of
        [] ->
            make_binary(Format);
        _ ->
            case (catch io_lib:format(Format, Data)) of
                {'EXIT', _} ->
                    iolist_to_binary(
                      io_lib:format("Format FAIL: io_lib:format(~p, ~p)",
                                    [Format, Data]));
                Msg ->
                    iolist_to_binary(Msg)
            end
    end.


force_binary(Arg) when is_binary(Arg) ->
    Arg;

force_binary(Arg) when is_list(Arg) ->
    list_to_binary(Arg).


make_binary(Input) ->
    try
        iolist_to_binary(Input)
    catch
        _:_ ->
            iolist_to_binary(io_lib:format("~p", [Input]))
    end.
