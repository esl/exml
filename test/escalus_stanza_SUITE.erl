-module(escalus_stanza_SUITE).
-compile(export_all).

-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(eq(E, A), ?assertEqual(E, A)).

all() ->
    [sanity_check,
     nullary_snippet_to_xmlel].

%%
%% Tests
%%

sanity_check(_) ->
    ok.

nullary_snippet_to_xmlel(_) ->
    M = escalus_stanza,
    ?eq(#xmlel{name = <<"el">>}, M:from_xml("<el/>")),
    ?eq(#xmlel{name = <<"el">>}, M:from_xml(<<"<el/>">>)).
