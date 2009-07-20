#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -sasl errlog_type error -boot start_sasl

main(_) ->
    {ok, {baberl, BaberlPid}} = baberl:start(),
    io:format("Starting ", []),
    stress(BaberlPid, 1000000),
    io:format(" Done.", []),
    ok.

stress(_, 0) -> ok;
stress(BaberlPid, Count) ->
    baberl:convert(BaberlPid, "", "UTF-8", <<"Hello, world">>),
    if
        Count rem 1000 == 0 ->
            erlang:garbage_collect(self()),
            io:format("."),
            timer:sleep(100);
        true -> ok
    end,
    stress(BaberlPid, Count - 1).
