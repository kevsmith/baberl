#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -sasl errlog_type error -boot start_sasl

main([FileName]) ->
    case erlang:open_port({spawn, "iconv -l"}, [line]) of
        Port when is_port(Port) ->
            file:write_file(FileName, generate_header_file(gather_output()));
        Error ->
            throw(Error)
    end.

%% Private function
generate_header_file(Output) ->
    Encodings = lists:usort(lists:sort(string:tokens(Output, " "))),
    [H|T] = Encodings,
    Defines = lists:foldr(fun(E, Acc) ->
        Acc ++ ",\"" ++ E ++ "\"" end,
    "-define(SUPPORTED_ENCODINGS, [\"" ++ H ++ "\"", T),
    Defines ++ ", \"\"]).\n".

gather_output() ->
    gather_output([]).

gather_output(Accum) ->
    receive
        {_Port, {data, {eol, Data}}} -> gather_output([Data ++ " "|Accum])
        after 100 -> lists:flatten(lists:reverse(Accum))
    end.
