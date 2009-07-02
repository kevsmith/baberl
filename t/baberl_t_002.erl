-module(baberl_t_002).

-export([start/0]).

start() ->
  etap:plan(7),
  etap:ok(ok =:= application:start(baberl), "Start baberl app"),
  etap:ok({ok, <<"foo">>} =:= baberl:convert("", "UTF-8", <<"foo">>), "Convert a default-encoded string"),
  etap:ok({ok, <<"foo">>} =:= baberl:convert("UTF-8", "ISO-8859-1", <<"foo">>), "Convert a UTF-8 encoded string"),
  etap:ok({ok, <<"foo">>} =:= baberl:convert("UTF-8", "ASCII", <<"foo">>), "Simple conversion works"),
  etap:ok({ok, <<"foo">>} =:= baberl:convert("UTF-8", "ASCII//translit//IGNORE", <<"foo">>), "Complex conversion works"),
  etap:ok({error, bad_input} =:= baberl:convert("UTF-8", "ASCII", <<"foo‘">>), "Bad encoding doesn't provoke segfault"),
  etap:ok({ok, <<"foo^O">>} =:= baberl:convert("UTF-8", "ASCII//translit//IGNORE", unicode:characters_to_binary("foo‘")), "Transliteration works"),
  etap:end_tests().
