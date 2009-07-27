-module(baberl_wordnet).

-behaviour(gen_server).

%% API
-export([start_link/0, lookup_plural/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {plurals=dict:new()}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

lookup_plural(PartOfSpeech, Word) when is_atom(PartOfSpeech) ->
  gen_server:call(?SERVER, {lookup_plural, PartOfSpeech, Word}).

init([]) ->
  {ok, #state{}}.

handle_call({lookup_plural, noun, Word}, _From, #state{plurals=Plurals}=State) ->
  {NewState, PluralList} = case dict:find(noun_plurals, Plurals) of
                             {ok, P} ->
                               {State, P};
                             _ ->
                               {ok, Bin} = file:read_file(plurals_file_name(noun)),
                               P = binary_to_term(Bin),
                               {State#state{plurals=dict:store(noun_plurals, P, Plurals)}, P}
                           end,
  {reply, binary_search(Word, split(PluralList)), NewState};

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal functions
plurals_file_name(PartOfSpeech) ->
  BaseDir = filename:join([filename:dirname(code:which(?MODULE)), "..", "priv"]),
  filename:join([BaseDir, atom_to_list(PartOfSpeech) ++ "_plurals.bin"]).

binary_search(_Word, not_found) ->
  not_found;
binary_search(Word, {T, {W, P}, B}) ->
  if
    Word =:= W ->
      P;
    Word > W ->
      binary_search(Word, split(B));
    Word < W ->
      binary_search(Word, split(T))
  end.

split([]) ->
  not_found;
split(Words) ->
  {T, B} = lists:split(erlang:round(length(Words) / 2) - 1, Words),
  [C|B1] = B,
  {T, C, B1}.
