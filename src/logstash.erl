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
          messages_dropped=false :: boolean(),
          encoder,
          timer}).


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


initialize() ->
    gen_server:cast(?SERVER, initialize).


send_raw(Message) ->
    gen_server:cast(?SERVER, {send, info, Message, []}).


send(Message) ->
    send(info, Message).


send(Severity, Message) ->
    send(Severity, Message, []).


send(Severity, Message, Extra) ->
    gen_server:cast(
      ?SERVER, {send, Severity, Message, Extra}).


deliver() ->
    gen_server:cast(?SERVER, deliver).


rotate_log() ->
    gen_server:cast(?SERVER, rotate_log).


init([]) ->
    initialize(),
    {ok, #state{encoder=get_encoder()}}.


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
    {noreply, New_state#state{timer=undefined}};

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
                Encoder = get_encoder(),
                case file:open(Filename, [write, raw, binary]) of
                    {ok, File} ->
                        deliver(),
                        State#state{file=File, file_size=0, encoder=Encoder};
                    _ ->
                        State#state{file=undefined, file_size=0,
                                    encoder=Encoder}
                end;
            false ->
                State
        end,
    {noreply, New_state};

handle_cast(initialize, State) ->
    {ok, _} = timer:apply_after(?SEND_INTERVAL, ?MODULE, deliver, []),
    {noreply, State#state{encoder=get_encoder()}};

handle_cast({send, Severity, Message, Extra},
            State=#state{messages=Messages, nr_messages=Nr_messages,
                         messages_dropped=Messages_dropped,
                         messages_size=Messages_size,
                         timer=Timer, encoder=Encoder}) ->
    Formatted =
        case Message of
            _ when is_binary(Message) ->
                Message;
            _ when is_list(Message) ->
                make_binary(Message);
            {logstash_format, Data} ->
                format(Data);
            {logstash_format, Format, Data} ->
                format(Format, Data);
            _ ->
                format(Message)
        end,

    Encoded = encode_message(Encoder, Severity, Formatted, Extra),

    New_state =
        if
            Nr_messages =< ?MAX_MESSAGES ->
                Part = iolist_to_binary([Encoded, $\n]),
                State#state{messages=[Part | Messages],
                            nr_messages=Nr_messages + 1,
                            messages_size=Messages_size + size(Part)};
            Nr_messages > ?MAX_MESSAGES ->
                if
                    Messages_dropped ->
                        State;
                    true ->
                        Error_msg = encode_message(
                                      Encoder,
                                      error,
                                      <<"Message buffer overflow">>,
                                      [{pid, conv_binary(self())},
                                       {module, ?MODULE},
                                       {file, conv_binary(?FILE)},
                                       {line, ?LINE}]),
                        State#state{messages=[[Error_msg, $\n] | Messages],
                                    messages_dropped=true,
                                    messages_size=(Messages_size +
                                                       size(Error_msg) + 1)}
                end
        end,
    New_timer =
        case Timer of
            undefined ->
                {ok, Tref} = timer:apply_after(
                               ?SEND_INTERVAL, ?MODULE, deliver, []),
                Tref;
            _ ->
                Timer
        end,
    {noreply, New_state#state{timer=New_timer}};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


encode_message(Encoder, Severity, Message, Extra) ->
    %% Numerical_severity = numerical_severity(Severity),
    Msg = [{severity, conv_binary(Severity)},
           {num_severity, numerical_severity(Severity)},
           {message, conv_binary(Message)} | Extra],
    iolist_to_binary(Encoder(Msg)).


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
    {Msg, _Size} = logstash_trunc_io:print(Arg, 10240),
    iolist_to_binary(Msg).


get_encoder() ->
    {ok, {Module, Function}} = application:get_env(logstash, encoder_factory),
    Module:Function().


format(Data) ->
    {ok, Truncate_messages} = application:get_env(logstash, truncate_messages),
    case Truncate_messages of
        false ->
            event:format("~p", [Data]);
        true ->
            {Msg, _Size} = logstash_trunc_io:print(Data, 10240),
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
