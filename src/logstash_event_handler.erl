-module(logstash_event_handler).
-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2,
         code_change/3]).


init(_) ->
    {ok, []}.


pid_info(Pid) ->
    [{pid, logstash:conv_binary(Pid)}].


handle_event({error, _Gleader, {Pid, Format, Data}}, State) when is_pid(Pid) ->
    logstash:send(error, {logstash_format, Format, Data}, pid_info(Pid)),
    {ok, State};

handle_event({error_report, _Gleader, {Pid, std_error, Report}}, State)
  when is_pid(Pid) ->
    logstash:send(error, {logstash_format, Report}, pid_info(Pid)),
    {ok, State};

handle_event({error_report, _Gleader, {Pid, Type, Report}}, State)
  when is_pid(Pid), is_atom(Type) ->
    logstash:send(Type, {logstash_format, Report}, pid_info(Pid)),
    {ok, State};

handle_event({warning_msg, _Gleader, {Pid, Format, Data}}, State)
  when is_pid(Pid) ->
    logstash:send(warning, {logstash_format, Format, Data}, pid_info(Pid)),
    {ok, State};

handle_event({warning_report, _Gleader, {Pid, std_warning, Report}}, State)
  when is_pid(Pid) ->
    logstash:send(warning, Report, pid_info(Pid)),
    {ok, State};

handle_event({warning_report, _Gleader, {Pid, Type, Report}}, State)
  when is_pid(Pid), is_atom(Type) ->
    logstash:send(Type, {logstash_format, Report}, pid_info(Pid)),
    {ok, State};

handle_event({info_msg, _Gleader, {Pid, Format, Data}}, State)
  when is_pid(Pid) ->
    logstash:send(info, {logstash_format, Format, Data}, pid_info(Pid)),
    {ok, State};

handle_event({info_report, _Gleader, {Pid, std_info, Report}}, State)
  when is_pid(Pid) ->
    logstash:send(info, {logstash_format, Report}, pid_info(Pid)),
    {ok, State};

handle_event({info_report, _Gleader, {Pid, Type, Report}}, State)
  when is_pid(Pid), is_atom(Type) ->
    logstash:send(Type, {logstash_format, Report}, pid_info(Pid)),
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
