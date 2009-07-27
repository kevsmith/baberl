#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa t/ -pa ebin -sasl errlog_type error -boot start_sasl

main(_) ->
  Words = [{"boy", "boys"},
           {"bed", "beds"},
           {"pencil", "pencils"},
           {"user", "users"},
           {"PC", "PCs"},
           {"Nick", "Nicks"},
           {"church", "churches"},
           {"edge", "edges"},
           {"mush", "mushes"},
           {"horse", "horses"},
           {"prize", "prizes"},
           {"spy", "spies"},
           {"poppy", "poppies"},
           {"penny", "pennies"},
           {"osprey", "ospreys"},
           {"bay", "bays"},
           {"calf", "calves"},
           {"elf", "elves"},
           {"half", "halves"},
           {"hoof", "hooves"},
           {"leaf", "leaves"},
           {"fireman", "firemen"},
           {"policeman", "policemen"},
           {"foot", "feet"},
           {"tooth", "teeth"},
           {"child", "children"},
           {"auto", "autos"},
           {"hero", "heroes"},
           {"potato", "potatoes"},
           {"echo", "echoes"},
           {"video", "videos"}],

  etap:plan(length(Words)),
  etap:is(application:start(baberl), ok, "Starting baberl"),
  lists:foreach(fun({Singular, Plural}) ->
                    etap:is(baberl_plurals:pluralize(noun, Singular), Plural, Singular ++ " -> " ++ Plural) end, Words),
  etap:end_tests().
