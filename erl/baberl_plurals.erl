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

%% Regexes and pluralization logic implemented using Joanna Crump's linguistic info:
%% http://www2.gsu.edu/~wwwesl/egw/crump.htm

pluralize(noun, Word) ->
  case baberl_wordnet:lookup_plural(noun, Word) of
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
