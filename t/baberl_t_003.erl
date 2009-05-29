-module(baberl_t_003).

-export([start/0]).

start() ->
  etap:plan(unknown),
  etap:ok(ok =:= application:start(baberl), "Start baberl app"),
  etap:ok(baberl:supported("UTF-8"), "UTF-8 is supported charset"),
  etap:not_ok(baberl:supported("ISO-8859"), "ISO-8859 isn't a supported charset"),
  etap:ok(baberl:supported("ISO-8859-1"), "ISO-8859-1 is a supported charset"),
  etap:ok("ISO-8859-1" =:= baberl:closest_match("ISO-8859"), "Finds closest matching charset"),
  etap:end_tests().