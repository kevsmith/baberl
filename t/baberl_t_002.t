#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa t/ -pa ebin -sasl errlog_type error -boot start_sasl

main(_) ->
  baberl_t_002:start().
