exml
====

[![GitHub Actions](https://github.com/esl/exml/workflows/ci/badge.svg?branch=master)](https://github.com/esl/exml/actions?query=workflow%3Aci+branch%3Amaster)
[![Codecov](https://codecov.io/gh/esl/exml/branch/master/graph/badge.svg)](https://codecov.io/gh/esl/exml)

**exml** is an Erlang library for parsing XML streams and doing complex XML structures manipulation.

Building
========

**exml** is a rebar3-compatible OTP application, run `make` or `./rebar3 compile` in order to build it. A C++11 compiler is required.


Using
=====

**exml** can parse both XML streams as well as single XML documents at once.

To parse a whole XML document:

```erlang
{ok, Parser} = exml:parse(<<"<my_xml_doc/>">>).
```

To generate an XML document from Erlang terms:

```erlang
El = #xmlel{name = <<"foo">>,
            attrs = [{<<"attr1">>, <<"bar">>}],
            children = [{xmlcdata, <<"Some Value">>}]},
exml:to_list(El).
```
or (pastable into `erl` shell):
```erlang
El = {xmlel, <<"foo">>,
      [{<<"attr1">>, <<"bar">>}],
      [{xmlcdata, <<"Some Value">>}]}.
exml:to_list(El).
```

Which results in:
```xml
<foo attr1='bar'>Some Value</foo>
```

`exml:to_binary/1` works similarly.

There's also `exml:to_pretty_iolist/1,3` for a quick'n'dirty document preview (pastable into `erl`):

```erlang
rr("include/exml.hrl").
El = #xmlel{name = <<"outer">>,
            attrs = [{<<"attr1">>, <<"val1">>},
                     {<<"attr2">>, <<"val-two">>}],
            children = [#xmlel{name = <<"inner-childless">>},
                        #xmlel{name = <<"inner-w-children">>,
                               children = [#xmlel{name = <<"a">>}]}]}.
io:format("~s", [exml:to_pretty_iolist(El)]).
```
which prints:
```xml
<outer attr2='val-two' attr1='val1'>
  <inner-childless/>
  <inner-w-children>
    <a/>
  </inner-w-children>
</outer>
```

For an example of using the streaming API see `test/exml_stream_tests.erl`.

XML Tree navigation
=====

The `exml_query` module exposes powerful helper functions to navigate the tree, please refer to the documentation available.
