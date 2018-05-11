# [3.0.0] 2018-05-04

## Changed

* Replaced expat with RapidXML

# [2.5.0] 2018-05-04

## Added

* A new API to query elements with specific namespaces. Path querying is extended with new selectors as well.
* Child element size (in bytes) limit may be configured.
* Stream opening tag is now configurable.

## Changed

* Testable with `rebar3`; cover is now enabled.

## Misc

* C sources reformatting.

# [2.4.1] 2016-12-17

## Changed

- C code building on OS X and FreeBSD

# [2.4.0] 2016-10-07

## Added

- support for Erlang/OTP 17 to 19

## Changed

- improved integration with `dialyzer`
- improved integration with `rebar3`

# [2.3.0] 2016-05-19

## Removed

- support for single `#xmlel` as a child

# [2.2.0] 2015-10-09

## Added

- escaping attr value: #10
- escaping data tags: #14

## Changed

- improved type specs: #14
- improved binary allocation: #14
- improved stanza size calculation: #14

# [2.1.5] 2014-09-29

## Fixed

- memory leak in `exml_event`: #8 by @RGafiyatullin

# [2.1.4] 2014-05-12

## Added

- support for XMPP over WebSockets as in [RFC 7395](https://tools.ietf.org/html/rfc7395)

# 2.1.0

- Pretty printing XML elements
