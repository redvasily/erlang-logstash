-module(logstash).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([conv_binary/1]).

-export([send_raw/1,
         send/1,
         send/2,
         send/3,
         deliver/0,
         rotate_log/0]).

-compile(export_all).

-define(SERVER, ?MODULE).


-define(MAX_FILE_SIZE, 100 * 1024 * 1024).
-define(MAX_MESSAGES, 1000000).
-define(SEND_INTERVAL, 1000).


-record(state, {
          file :: file:io_device(),
          file_size :: integer(),
          messages=[] :: [iolist()],
          nr_messages=0 :: integer(),
          messages_size=0 :: integer(),
          messages_dropped=false :: boolean()}).


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


initialize() ->
    gen_server:cast(?SERVER, initialize).


send_raw(Message) ->
    gen_server:cast(?SERVER, {send_raw, conv:binary(Message)}).


send(Message) ->
    send(info, Message).


send(Severity, Message) ->
    send(Severity, Message, []).


send(Severity, Message, Extra) ->
    gen_server:cast(
      ?SERVER, {send_raw, format_message(Severity, Message, Extra)}).


deliver() ->
    gen_server:cast(?SERVER, deliver).


rotate_log() ->
    gen_server:cast(?SERVER, rotate_log).


init([]) ->
    initialize(),
    {ok, #state{}}.


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(deliver, State=#state{file=File, file_size=File_size,
                                  messages=Messages,
                                  messages_size=Messages_size}) ->
    New_file =
        case File of
            undefined ->
                undefined;
            _ ->
                case file:write(File, lists:reverse(Messages)) of
                    ok ->
                        File;
                    {error, _} ->
                        file:close(File),
                        undefined
                end
        end,
    New_state =
        case New_file of
            undefined ->
                rotate_log(),
                State#state{file=undefined};
            _ ->
                New_file_size = File_size + Messages_size,
                case New_file_size > ?MAX_FILE_SIZE of
                    true ->
                        file:close(File),
                        rotate_log(),
                        State#state{file=undefined,
                                    messages=[],
                                    nr_messages=0,
                                    messages_size=0,
                                    file_size=undefined};
                    false ->
                        State#state{
                          messages=[],
                          nr_messages=0,
                          messages_size=0,
                          file_size=New_file_size}
                end
        end,
    {noreply, New_state};

handle_cast(rotate_log, State) ->
    New_state =
        case (State#state.file =:= undefined orelse
              State#state.file_size > ?MAX_FILE_SIZE) of
            true ->
                case State#state.file of
                    undefined ->
                        pass;
                    _ ->
                        file:close(State#state.file)
                end,
                {ok, Filename} = application:get_env(filename),
                file:rename(Filename ++ ".1", Filename ++ ".2"),
                file:rename(Filename, Filename ++ ".1"),
                case file:open(Filename, [write, raw, binary]) of
                    {ok, File} ->
                        deliver(),
                        State#state{file=File, file_size=0};
                    _ ->
                        State#state{file=undefined, file_size=0}
                end;
            false ->
                State
        end,
    {noreply, New_state};

handle_cast(initialize, State) ->
    {ok, _} = timer:apply_after(?SEND_INTERVAL, ?MODULE, deliver, []),
    {noreply, State#state{}};

handle_cast({send_raw, Message_raw},
            State=#state{messages=Messages, nr_messages=Nr_messages,
                         messages_dropped=Messages_dropped,
                         messages_size=Messages_size}) ->
    New_state =
        if
            Nr_messages =< ?MAX_MESSAGES ->
                Part = iolist_to_binary([Message_raw, $\n]),
                State#state{messages=[Part | Messages],
                            nr_messages=Nr_messages + 1,
                            messages_size=Messages_size + size(Part)};
            Nr_messages > ?MAX_MESSAGES ->
                if
                    Messages_dropped ->
                        State;
                    true ->
                        Error_msg = format_message(
                                      error,
                                      <<"Message buffer overflow">>,
                                      [{pid, conv_binary(self())},
                                       {module, ?MODULE},
                                       {file, ?FILE}, {line, ?LINE}]),
                        State#state{messages=[[Error_msg, $\n] | Messages],
                                    messages_dropped=true,
                                    messages_size=(Messages_size +
                                                       size(Error_msg) + 1)}
                end
        end,
    {ok, _} = timer:apply_after(?SEND_INTERVAL, ?MODULE, deliver, []),
    {noreply, New_state};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


format_message(Severity, Message, Extra) ->
    %% Numerical_severity = numerical_severity(Severity),
    Msg = [{severity, conv_binary(Severity)},
           {num_severity, numerical_severity(Severity)},
           {message, conv_binary(Message)} | Extra],
    iolist_to_binary(mochijson2:encode(Msg)).


numerical_severity(debug) ->
    10;

numerical_severity(info) ->
    20;

numerical_severity(info_report) ->
    20;

numerical_severity(notice) ->
    30;

numerical_severity(warning) ->
    40;

numerical_severity(warning_report) ->
    40;

numerical_severity(error) ->
    50;

numerical_severity(error_report) ->
    50;

numerical_severity(_) ->
    10.


conv_binary(Arg) when is_binary(Arg) ->
    Arg;

conv_binary(Arg) when is_list(Arg) ->
    list_to_binary(Arg);

conv_binary(Arg) when is_atom(Arg) ->
    list_to_binary(atom_to_list(Arg));

conv_binary(Arg) when is_pid(Arg) ->
    list_to_binary(pid_to_list(Arg));

conv_binary(Arg) ->
    {Msg, _Size} = trunc_io:print(Arg, 10240),
    iolist_to_binary(Msg).
