-include("exml.hrl").

-record(xmlstreamstart, {name :: binary(),
                         attrs = #{} :: exml:attrs()}).

-record(xmlstreamend, {name :: binary()}).
