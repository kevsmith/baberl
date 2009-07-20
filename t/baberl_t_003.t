#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa t/ -pa ebin -sasl errlog_type error -boot start_sasl

main(_) ->
    etap:plan(5),
    etap:ok(ok =:= application:start(baberl), "Start baberl app"),
    etap:ok(baberl:is_supported_encoding("UTF-8"), "UTF-8 is supported charset"),
    etap:not_ok(baberl:is_supported_encoding("ISO-8859"), "ISO-8859 isn't a supported charset"),
    etap:ok(baberl:is_supported_encoding("ISO-8859-1"), "ISO-8859-1 is a supported charset"),
    etap:ok("ISO-8859-1" =:= baberl:closest_match("ISO-8859"), "Finds closest matching charset"),
    etap:end_tests().
