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

-define(EN_NOUN_PLURALIZERS, [{"^[A-Z]", fun(W) -> W ++ "s" end},
                         {"man\$", "men"},
                         {"se\$", "ses"},
                         {"ge\$", "ges"},
                         {"ch\$", "ches"},
                         {"sh\$", "shes"},
                         {"ze\$", "zes"},
                         {"x\$", "xes"},
                         {"[^aeiou]y", fun(W) -> {Root, _} = lists:split(length(W) - 1, W),
                                                   Root ++ "ies" end}]).

-define(EN_VERB_PLURALIZERS,
        [{irregular_pres, [{"am", "are"}, {"are", "are"},
                           {"is", "are"}, {"was", "were"},
                           {"were", "were"}, {"was", "were"},
                           {"have", "have"}, {"has", "have"}]},
         {ambiguous_pres, [{"act", "act"}, {"acts", "act"},
                           {"blame", "blame"}, {"blames", "blame"},
                           {"can", "can"}, {"must", "must"},
                           {"fly", "fly"}, {"flies", "fly"},
                           {"copy", "copy"}, {"copies", "copy"},
                           {"drink", "drink"}, {"drinks", "drink"},
                           {"fight", "fight"}, {"fights", "fight"},
                           {"fire", "fire"}, {"fires", "fire"},
                           {"like", "like"}, {"likes", "like"},
                           {"look", "look"}, {"looks", "look"},
                           {"make", "make"}, {"makes", "make"},
                           {"reach", "reach"}, {"reaches", "reach"},
                           {"run", "run"}, {"runs", "run"},
                           {"sink", "sink"}, {"sinks", "sink"},
                           {"sleep", "sleep"}, {"sleeps", "sleep"},
                           {"view", "view"}, {"views", "view"}]},
         {irregular_non_pres, ["did", "had", "ate", "made", "put", "spent",
                               "fought", "sanks", "gave", "sought",
                               "shall", "could", "ought", "should",
                               "thought", "saw", "bent", "will", "might",
                               "cut"]}]).

-define(EN_VERB_LIST_ORDER, [irregular_pres, ambiguous_pres, irregular_non_pres]).

%% Regexes and noun pluralization logic implemented using Joanna Crump's linguistic info:
%% http://www2.gsu.edu/~wwwesl/egw/crump.htm

%% @spec pluralize(PartOfSpeech, Word) -> Result
%%       PartOfSpech = noun | verb
%%       Word = string()
%%       Result = string()
%% @doc Analyze a noun and generate the plural form<br />
%%      <em>Note: Limited support for verbs</em>
pluralize(noun, Word) ->
  case baberl_wordnet:plural(noun, Word) of
    not_found ->
      analyze(Word);
    Plural ->
      Plural
  end;
pluralize(verb, Word) ->
  case find_verb(?EN_VERB_LIST_ORDER, Word) of
    not_found ->
      Word;
    Plural ->
      Plural
  end.

%% Internal functions
analyze(Word) ->
  analyze(Word, ?EN_NOUN_PLURALIZERS).

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

find_verb([], _Word) ->
  not_found;
find_verb([H|T], Word) ->
  Verbs = proplists:get_value(H, ?EN_VERB_PLURALIZERS),
  case find_matching_verb(Verbs, Word) of
    not_found ->
      find_verb(T, Word);
    Plural ->
      Plural
  end.

find_matching_verb([], _Word) ->
  not_found;
find_matching_verb([{Pattern, Plural}|T], Word) ->
  case re:run(Pattern, Word) of
    nomatch ->
      find_matching_verb(T, Word);
    _ ->
      Plural
  end;
find_matching_verb([PatternAndVerb|T], Word) ->
  case re:run(PatternAndVerb, Word) of
    nomatch ->
      find_matching_verb(T, Word);
    _ ->
      PatternAndVerb
  end.
