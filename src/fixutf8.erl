-module(fixutf8).

-export([fix_binary/1]).


-spec fix_binary(binary()) -> binary().
fix_binary(Input) ->
    fix_binary_(iolist_to_binary(Input), unicode, []).


fix_binary_(<<>>, _In_encoding, Acc) ->
    iolist_to_binary(lists:reverse(Acc));

fix_binary_(Input, In_encoding, Acc) ->
    case unicode:characters_to_binary(Input, In_encoding) of
        {_, Encoded, Rest} ->
            fix_binary_(binary:part(Input, {1, size(Rest) - 1}), In_encoding,
                     [<<"?">>, Encoded | Acc]);
        Result ->
            fix_binary_(<<>>, In_encoding, [Result | Acc])
    end.
