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

-module(baberl_driver).

-author("Kevin A. Smith <kevin@hypotheticalabs.com").

-include("baberl.hrl").

-export([load/0, unload/0, encodings/0, is_encoding_supported/1, convert/3]).

load() ->
  SearchDir = filename:join([filename:dirname(code:which(?MODULE)), "..", "priv"]),
  erl_ddll:load(SearchDir, "baberl_drv").

unload() ->
  erl_ddll:unload("baberl_drv").

encodings() ->
  ?SUPPORTED_ENCODINGS.

is_encoding_supported(Encoding) ->
  case lists:member(Encoding, ?SUPPORTED_ENCODINGS) of
    false ->
      lists:member(Encoding ++ "//", ?SUPPORTED_ENCODINGS);
    true ->
      true
  end.

convert(FromEncoding, ToEncoding, Text) when is_list(FromEncoding),
                                             is_list(ToEncoding),
                                             is_binary(Text) ->
  FromEncodingName = extract_encoding_name(FromEncoding),
  ToEncodingName = extract_encoding_name(ToEncoding),
  check_encodings([FromEncodingName, ToEncodingName]),
  P= connect_to_driver(),
  try
    perform_conversion(P, build_command(list_to_binary(FromEncoding), list_to_binary(ToEncoding), Text))
  catch
    %% Catch and rethrow so we can define an after clause
    %% to clean up the allocated port
    _:Error ->
      throw(Error)
  after
    port_close(P)
  end.

%% Private functions
extract_encoding_name(Encoding) when length(Encoding) == 0 ->
  "";
extract_encoding_name(Encoding) ->
  [EncodingName|_] = string:tokens(Encoding, "//"),
  EncodingName.

perform_conversion(P, Command) ->
  port_command(P, Command),
  receive
    R ->
      R
  after 50 ->
      {error, timeout}
  end.

build_command(From, To, Text) ->
  list_to_binary(lists:flatten(lists:map(fun(Term) ->
                                             S = size(Term),
                                             [<<S:32>>, Term] end, [From, To, Text]))).

connect_to_driver() ->
  open_port({spawn, baberl_drv}, [binary]).

check_encodings(Encodings) ->
  lists:foreach(fun(E) ->
                    case is_encoding_supported(E) =:= true orelse E =:= "" of
                      true ->
                        ok;
                      false ->
                        throw({error, {unsupported_encoding, E}})
                    end end, Encodings).
