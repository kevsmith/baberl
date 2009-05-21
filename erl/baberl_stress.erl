-module(baberl_stress).

-define(TEST_RUNS, 1000000).

-compile([export_all]).

run() ->
  baberl_driver:load(),
  stress(?TEST_RUNS),
  baberl_driver:unload().

stress(0) ->
  ok;

stress(Count) ->
  baberl_driver:convert("", "UTF-8", <<"Hello, world">>),
  if
    Count rem 1000 == 0 ->
      erlang:garbage_collect(self()),
      io:format("."),
      timer:sleep(100);
    true ->
      ok
  end,
  stress(Count - 1).
