-module(baberl_t_002).

-export([start/0]).

start() ->
  etap:plan(unknown),
  etap:ok(ok =:= application:start(baberl), "Start baberl app"),
  %etap:ok({ok, <<"foo">>} =:= baberl:convert("", "UTF-8", <<"foo">>), "Convert a default-encoded string"),
  etap:ok({ok, <<"foo">>} =:= baberl:convert("UTF-8", "ISO-8859-1", <<"foo">>), "Convert a UTF-8 encoded string"),
  etap:ok({ok, <<"foo">>} =:= baberl:convert("UTF-8", "ASCII", <<"foo">>), "Simple conversion works"),
  etap:ok({ok, <<"foo">>} =:= baberl:convert("UTF-8", "ASCII//translit//IGNORE", <<"foo">>), "Complex conversion works"),
  etap:end_tests().
