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

-module(baberl_numbers).

-export([ordinal/1, ordinate/1]).

%% @spec ordinal(N) -> Result
%%       N = number()
%%       Result = string()
%% @doc Append the correct ordinal suffix onto a number<br />
%%      <ul>
%%      <li>Examples: 1 -> "1st", 3 -> "3rd", etc</li>
%%      </ul>
ordinal(N) when is_number(N) ->
  Suffix = case nth(N rem 100) of
             not_found ->
               nth(N rem 10);
             S ->
               S
           end,
  lists:flatten([integer_to_list(N), Suffix]).

%% @spec ordinate(N) -> Result
%%       N = number()
%%       Result = string()
%% @doc Turns the number into the equivalent English phrase<br />
%%      <ul>
%%      <li>Supports numbers up to 21 digits</li>
%%      <li>Examples: 1 -> "one", 123 -> "one hundred twenty three"</li>
%%      </ul>
ordinate(N) ->
  Chunks = chunkify(N),
  re:replace(string:strip(lists:flatten(ordinate_chunks(Chunks))), "  ", " ", [{return, list}]).

%% Internal functions

nth(0) ->
  "th";
nth(1) ->
  "st";
nth(2) ->
  "nd";
nth(3) ->
  "rd";
nth(N) when N > 3 andalso N < 14 ->
  "th";
nth(_) ->
  not_found.

chunkify(N) ->
  chunkify(lists:reverse(integer_to_list(N)), []).

chunkify([], Acc) ->
  lists:reverse(Acc);
chunkify([H1, H2, H3|T], Acc) ->
  chunkify(T, [{[H3, H2, H1]}|Acc]);
chunkify([H1, H2], Acc) ->
  chunkify([], [{[H2, H1]}|Acc]);
chunkify([H1], Acc) ->
  chunkify([], [{H1}|Acc]).

ordinate_chunks(Chunks) ->
  ordinate_chunks(Chunks, 0, []).

ordinate_chunks([], _, Accum) ->
  Accum;
ordinate_chunks([{H}|T], 0, Accum) ->
  ordinate_chunks(T, 1, [ordinate_chunk(H)|Accum]);
ordinate_chunks([{H}|T], Pos, Accum) ->
  case string:strip(ordinate_chunk(H)) of
    "" ->
      ordinate_chunks(T, Pos + 1, Accum);
    C ->
      ordinate_chunks(T, Pos + 1, [[C, " ", placename(Pos), " "]|Accum])
  end.

ordinate_chunk([Hundreds, Tens, Ones]) ->
  lists:flatten([numword(list_to_integer([Hundreds]), hundreds),
                 " ",
                 ordinate_chunk([Tens, Ones])]);
ordinate_chunk([Tens, Ones]) when Tens > $1 ->
  lists:flatten([numword(list_to_integer([Tens]), tens), " ", numword(list_to_integer([Ones]))]);
ordinate_chunk([_Tens, _Ones]=N) ->
  numword(list_to_integer(N));
ordinate_chunk(Ones) ->
  numword(list_to_integer([Ones])).

placename(0) ->
  "";
placename(1) ->
  "thousand";
placename(2) ->
  "million";
placename(3) ->
  "bilion";
placename(4) ->
  "trilion";
placename(5) ->
  "quadrillion";
placename(6) ->
  "quntillion";
placename(7) ->
  "sextillion".

numword(0) ->
  "";
numword(1) ->
  "one";
numword(2) ->
  "two";
numword(3) ->
  "three";
numword(4) ->
  "four";
numword(5) ->
  "five";
numword(6) ->
  "six";
numword(7) ->
  "seven";
numword(8) ->
  "eight";
numword(9) ->
  "nine";
numword(10) ->
  "ten";
numword(11) ->
  "eleven";
numword(12) ->
  "twelve";
numword(13) ->
  "thirteen";
numword(14) ->
  "fourteen";
numword(15) ->
  "fifteen";
numword(16) ->
  "sixteen";
numword(17) ->
  "seventeen";
numword(18) ->
  "eighteen";
numword(19) ->
  "nineteen".

numword(0, tens) ->
  "";
numword(2, tens) ->
  "twenty";
numword(3, tens) ->
  "thirty";
numword(4, tens) ->
  "forty";
numword(5, tens) ->
  "fifty";
numword(6, tens) ->
  "sixty";
numword(7, tens) ->
  "seventy";
numword(8, tens) ->
  "eighty";
numword(9, tens) ->
  "ninety";
numword(0, hundreds) ->
  "";
numword(N, hundreds) when N > 0 andalso N < 10 ->
  lists:flatten([numword(N), " hundred"]).
