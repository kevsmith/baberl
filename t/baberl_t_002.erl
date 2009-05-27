-module(baberl_t_002).

-export([start/0]).

start() ->
  etap:plan(unknown),
  etap:ok(ok =:= application:start(baberl), "Start baberl app"),
  etap:ok(<<"foo">> =:= baberl:convert("", "UTF-8", <<"foo">>), "Convert a string"),
  etap:end_tests().
