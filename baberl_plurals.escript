#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -sasl errlog_type error -boot start_sasl

main(_) ->
  {ok, File} = file:read_file("noun.exc"),
  Lines = string:tokens(binary_to_list(File), "\n"),
  Words = lists:usort(lists:flatten([parse(Line) || Line <- Lines])),
  file:write_file(filename:join("priv", "noun_plurals.bin"), term_to_binary(Words)),
  ok.

parse(Line) ->
  [Plural|Roots] = string:tokens(Line, " "),
  [{R, Plural} || R <- Roots].
