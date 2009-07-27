# About

Baberl is an interface to the iconv library.

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

Hurray!

The baberl.app file is also included for OTP distribution.

# License

Copyright (c) 2009 Electronic Arts, Inc.

This library was developed by Electronic Arts and is open source under the MIT license.

# Credits

Kevin A. Smith <kevin@hypotheticalabs.com><br/>
Nick Gerakines <nick@gerakines.net>
