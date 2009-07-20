#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -sasl errlog_type error -boot start_sasl

main(_) ->
    etap:plan(unknown),
    etap:is(application:start(baberl), ok, "Start baberl app"),
    etap:is(application:stop(baberl), ok, "Stop baberl app"),
    etap:end_tests().
