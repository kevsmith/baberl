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

-module(baberl).

-author("Kevin A. Smith <kevin@hypotheticalabs.com").

-behaviour(gen_server).
-include("baberl.hrl").

%% API
-export([start_link/0, convert/2, convert/3, encodings/0, is_supported/1]).
-export([closest_match/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%---------------------------------------------------------
%% @doc
%% Starts the baberl server process
%%
%% @spec start_link() -> {ok, pid()} | {error, atom()}
%%----------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], [{fullsweep_after, 0}]).

%%---------------------------------------------------------
%% @doc
%% Takes an Erlang binary and converts it from one
%% charset encoding to another
%%
%% @spec convert(string(), string(), binary()) -> binary()
%%----------------------------------------------------------
convert(FromEncoding, ToEncoding, Text) when is_list(FromEncoding),
                                             is_list(ToEncoding),
                                             is_binary(Text) ->
  Result = gen_server:call(?MODULE, {convert, FromEncoding, ToEncoding, Text}),
  case Result of
    {error, {unsupported_encoding, _}} ->
      throw(Result);
    _ ->
      Result
  end.

convert(ToEncoding, Text) when is_list(ToEncoding),
                               is_binary(Text) ->
  convert("", ToEncoding, Text).

encodings() ->
  gen_server:call(?MODULE, encodings).

is_supported(Encoding) ->
  gen_server:call(?MODULE, {is_supported, Encoding}).

closest_match(Encoding) ->
  case is_supported(Encoding) of
    true ->
      Encoding;
    false ->
      gen_server:call(?MODULE, {closest_match, Encoding})
  end.

%% gen_server callbacks
init([]) ->
  process_flag(trap_exit, true),
  ok = baberl_driver:load(),
  {ok, []}.

handle_call({closest_match, Encoding}, From, State) ->
  proc_lib:spawn_link(fun() ->
                          case lists:reverse([E || E <- ?SUPPORTED_ENCODINGS,
                                                   string:str(E, Encoding) == 1]) of
                            [] ->
                              gen_server:reply(From, []);
                            [H|_] ->
                              gen_server:reply(From, H)
                          end end),
  {noreply, State};

handle_call(encodings, _From, State) ->
  {reply, baberl_driver:encodings(), State};

handle_call({is_supported, Encoding}, _From, State) ->
  {reply, baberl_driver:is_encoding_supported(Encoding), State};

handle_call({convert, FromEncoding, ToEncoding, Text}, From, State) ->
  proc_lib:spawn_link(fun() ->
                          Result = try
                                     begin
                                       case unicode:characters_to_list(Text, utf8) of
                                         {incomplete, _, _} ->
                                           {error, bad_input};
                                         _ ->
                                           baberl_driver:convert(FromEncoding, ToEncoding, Text)
                                       end
                                     end
                                   catch
                                     throw: Error ->
                                       Error
                                   end,
                          gen_server:reply(From, Result) end),
  {noreply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  baberl_driver:unload().

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
