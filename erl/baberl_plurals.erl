%% Copyright (c) 2009 Electronic Arts, Inc.

%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

-module(baberl_plurals).

-export([pluralize/2]).

-define(EN_PLURALIZERS, [{"^[A-Z]", fun(W) -> W ++ "s" end},
                         {"man\$", "men"},
                         {"se\$", "ses"},
                         {"ge\$", "ges"},
                         {"ch\$", "ches"},
                         {"sh\$", "shes"},
                         {"ze\$", "zes"},
                         {"x\$", "xes"},
                         {"[^aeiou]y", fun(W) -> {Root, _} = lists:split(length(W) - 1, W),
                                                   Root ++ "ies" end}]).

%% Regexes and noun pluralization logic implemented using Joanna Crump's linguistic info:
%% http://www2.gsu.edu/~wwwesl/egw/crump.htm

%% @spec pluralize(PartOfSpeech, Word) -> Result
%%       PartOfSpech = noun
%%       Word = string()
%%       Result = string()
%% @doc Analyze a noun and generate the plural form
pluralize(noun, Word) ->
  case baberl_wordnet:plural(noun, Word) of
    not_found ->
      analyze(Word);
    Plural ->
      Plural
  end.

%% Internal functions
analyze(Word) ->
  analyze(Word, ?EN_PLURALIZERS).

analyze(Word, []) ->
  Word ++ "s";
analyze(Word, [{Pattern, Action}|T]) ->
  case regexp:match(Word, Pattern) of
    {match, _, _} ->
      exec_action(Word, Pattern, Action);
    _ ->
      analyze(Word, T)
  end.

exec_action(Word, _Pattern, Action) when is_function(Action) ->
  Action(Word);
exec_action(Word, Pattern, Action) ->
  {ok, Plural, _} = regexp:sub(Word, Pattern, Action),
  Plural.
