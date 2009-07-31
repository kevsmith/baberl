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
-include("baberl.hrl").

-export([convert/2, convert/3, check_encodings/1, is_supported_encoding/1]).
-export([encodings/0, closest_match/1]).

%% @equiv convert(Pid, "", ToEncoding, Text)
convert(ToEncoding, Text) when is_list(ToEncoding), is_binary(Text) ->
  convert("", ToEncoding, Text).

%% @spec convert(FromEncoding, ToEncoding, Text) -> Result
%%       FromEncoding = string()
%%       ToEncoding = string()
%%       Text = binary()
%%       Result = term()
%% @doc Converts text from one encoding to another
convert(FromEncoding, ToEncoding, Text) when is_list(FromEncoding), is_list(ToEncoding), is_binary(Text) ->
  case unicode:characters_to_list(Text, utf8) of
    {incomplete, _, _} -> throw(bad_input);
    _ -> ok
  end,
  check_encodings([extract_encoding_name(FromEncoding), extract_encoding_name(ToEncoding)]),
  Command = iolist_to_binary(lists:map(fun
                                       ("") -> [<<0:32>>, []];
                                       (Term) -> S = size(Term), [<<S:32>>, Term]
                                      end, [list_to_binary(FromEncoding), list_to_binary(ToEncoding), Text])),
  Port = start_port(),
  try
    begin
      port_command(Port, Command),
      receive
        R ->
          R
      after 50 ->
        {error, timeout}
      end
    end
  after
    port_close(Port)
  end.

%% @spec check_encodings(Encodings) -> Result
%%       Encodings = list(string())
%%       Result = ok
%% @throws tuple(unsupported_encoding, string())
%% @doc Verifies all encodings are supported by the underlying iconv machinery
check_encodings(Encodings) ->
  lists:foreach(fun(E) ->
                    case is_supported_encoding(E) =:= true orelse E =:= "" of
                      true -> ok;
                      false -> throw({error, {unsupported_encoding, E}})
                    end
                end, Encodings).

%% @spec is_supported_encoding(Encoding) -> Result
%%       Encoding = string()
%%       Result = true | false
%% @doc Determines if iconv supports a specific encoding
is_supported_encoding(Encoding) ->
  lists:member(Encoding, ?SUPPORTED_ENCODINGS) or lists:member(Encoding ++ "//", ?SUPPORTED_ENCODINGS).

%% @spec encodings() -> Result
%%       Result = list(string())
%% @doc Dumps the entire list of supported encodings
encodings() ->
  ?SUPPORTED_ENCODINGS.

%% @spec closest_match(Encoding) -> Result
%%       Encoding = string()
%%       Result = [] | string()
%% @doc Finds the closest encoding based on _name_ not charset similarity
closest_match(Encoding) ->
  case is_supported_encoding(Encoding) of
    true -> Encoding;
    false ->
      case lists:reverse([E || E <- ?SUPPORTED_ENCODINGS, string:str(E, Encoding) == 1]) of
        [] -> [];
        [H | _] -> re:replace(H, "//", "", [{return, list}])
      end
  end.

%% @private
extract_encoding_name("") -> "";
extract_encoding_name(Encoding) ->
  [EncodingName | _] = string:tokens(Encoding, "//"),
  EncodingName.

%% @private
start_port() ->
  open_port({spawn, 'baberl_drv'}, [binary]).
