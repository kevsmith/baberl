#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa t/ -pa ebin -sasl errlog_type error -boot start_sasl

main(_) ->
  Ordinals = [{1, "1st"}, {2, "2nd"},
              {3, "3rd"}, {4, "4th"},
              {5, "5th"}, {6, "6th"},
              {7, "7th"}, {8, "8th"},
              {9, "9th"}, {10, "10th"},
              {11, "11th"}, {12, "12th"},
              {13, "13th"}, {14, "14th"},
              {15, "15th"}, {16, "16th"},
              {17, "17th"}, {18, "18th"},
              {19, "19th"}, {20, "20th"},
              {21, "21st"}, {23, "23rd"},
              {45, "45th"}, {100, "100th"},
              {103, "103rd"}, {1027, "1027th"}],
  Ordinates = [{1, "one"}, {2, "two"},
               {3, "three"}, {4, "four"},
               {123456789, "one hundred twenty three million four hundred fifty six thousand seven hundred eighty nine"},
               {1001, "one thousand one"},
               {1000001, "one million one"}],
  etap:plan(unknown),
  etap:is(application:start(baberl), ok, "Starting baberl"),
  lists:foreach(fun({Num, Ordinal}) ->
                    etap:is(baberl_numbers:ordinal(Num), Ordinal, integer_to_list(Num) ++ " -> " ++ Ordinal) end, Ordinals),
  {S1, S2, S3} = erlang:now(),
  random:seed(S1, S2, S3),
  etap:ok(is_list(baberl_numbers:ordinal(random:uniform(1000000000000))), "Can ordinal-ize random number"),
  lists:foreach(fun({N, W}) ->
                   etap:is(baberl_numbers:ordinate(N), W, integer_to_list(N) ++ " -> " ++ W) end, Ordinates),
  etap:end_tests().
