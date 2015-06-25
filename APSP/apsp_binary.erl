-module(apsp_binary).

-compile(export_all).

make_bin([]) ->
    <<>>;
make_bin(Data) ->
    {IntFromStr, _} = string:to_integer(hd(Data)),
    Bin = binary:encode_unsigned(IntFromStr,little),
    Rest = make_bin(tl(Data)),
    <<Bin/binary,Rest/binary>>.

get_data(Fd) ->
    case file:read_line(Fd) of 
        {ok, Data} ->
            Bin = make_bin(string:tokens(Data," ")),
            Rest = get_data(Fd),
            <<Bin/binary, Rest/binary>>;
        {error, Report} ->
            Report;
        eof ->
            <<>>
    end.
