# About

Baberl is a collection of features to make working with character sets and language easier. Right now, Baberl
provides two main features:

* Character set conversions via iconv
* Robust pluralization support for English nouns
* Basic pluralization support for English verbs
* Ordinal conversion for numbers
* Number-to-English (ordinate) conversion for whole numbers up to 21 digits

# Usage

Using it is simple.

    1> application:start(baberl).
    ok
    2> baberl:convert("", "UTF-8", <<"foo">>).
    {ok,<<"foo">>
    3> baberl:convert("UTF-8", "ASCII//translit//IGNORE", unicode:characters_to_binary("foo‘")).
    {ok,<<"foo^O">>}
    4> baberl_plurals:pluralize(noun, "cat").
    "cats"
    5> baberl_plurals:pluralize(noun, "half").
    "halves"
    6> baberl_plurals:pluralize(verb, "am").
    "are"
    7> baberl_plurals:pluralize(verb, "ran").
    "ran"
    8> baberl_numbers:ordinal(123).
    "123rd"
    9> baberl_numbers:ordinate(1001).
    "one thousand one"
    10> baberl_numbers:ordinate(123456789).
    "one hundred twenty three million four hundred fifty six thousand seven hundred eighty nine"

Hurray!

The baberl.app file is also included for OTP distribution.

# License

Copyright (c) 2009 Electronic Arts, Inc.

This library was developed by Electronic Arts and is open source under the MIT license.

# Credits

Kevin A. Smith <kevin@hypotheticalabs.com><br/>
Nick Gerakines <nick@gerakines.net>
