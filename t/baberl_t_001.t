#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pz t/ -pz ebin -sasl errlog_type error -boot start_sasl

main(_) ->
  baberl_t_001:start().
