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

%% API
-export([start_link/0, convert/3, encodings/0, is_supported/1]).

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
  gen_server:call(?MODULE, {convert, FromEncoding, ToEncoding, Text}).

encodings() ->
  gen_server:call(?MODULE, encodings).

is_supported(Encoding) ->
  gen_server:call(?MODULE, {is_supported, Encoding}).

%% gen_server callbacks
init([]) ->
  ok = baberl_driver:load(),
  {ok, []}.

handle_call(encodings, _From, State) ->
  {reply, baberl_driver:encodings(), State};

handle_call({is_supported, Encoding}, _From, State) ->
  {reply, baberl_driver:is_encoding_supported(Encoding), State};

handle_call({convert, From, To, Text}, From, State) ->
  proc_lib:spawn_opt(fun() ->
                         Result = baberl_driver:convert(From, To, Text),
                         gen_server:reply(From, Result) end, [{fullsweep_after, 0}]),
  {noreply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  baberl_driver:unload().

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
