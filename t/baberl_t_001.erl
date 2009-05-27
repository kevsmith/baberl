-module(baberl_t_001).

-export([start/0]).

start() ->
  etap:plan(unknown),
  etap:ok(ok =:= application:start(baberl), "Start baberl app"),
  etap:ok(ok =:= application:stop(baberl), "Stop baberl app"),
  etap:end_tests().
