%%%-------------------------------------------------------------------
%%% @author Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc Unit tests for exml application
%%%
%%% @end
%%% Created : 12 Jul 2011 by Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%%-------------------------------------------------------------------

-module(exml_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("exml/include/exml_stream.hrl").

-compile(export_all).

application_test() ->
    ?assertEqual(ok, application:start(exml)),
    ?assertEqual(ok, application:stop(exml)).

size_of_normal_xml_test() ->
    Raw = <<"<a attr=\"urn:test:0\"><b>grzegrzółka</b><c>&amp;</c></a>"/utf8>>,
    ?assertEqual(iolist_size(Raw), exml:xml_size(parse(Raw))).

size_of_escaped_characters_test() ->
    Raw = <<"<a>&amp;</a>">>,
    ?assertEqual(iolist_size(Raw), exml:xml_size(parse(Raw))).

to_binary_with_cdata_test() ->
    Raw = <<"<a><![CDATA[ Within this Character Data block I can ",
            "use double dashes as much as I want (along with <, &, ', and \")]]></a>">>,
    ?assertEqual(Raw, exml:to_iolist(parse(Raw), not_pretty, node_cdata)).

size_of_exml_with_cdata_test() ->
    Raw = <<"<a><![CDATA[ Within this Character Data block I can
            use double dashes as much as I want (along with <, &, ', and \")]]></a>">>,
    ?assertEqual(iolist_size(exml:to_binary(parse(Raw))), exml:xml_size(parse(Raw))).

sort_xmlel_identity_test() ->
    El = #xmlel{
            name = <<"foo">>,
            attrs = [{<<"attr1">>, <<"bar">>}],
            children = [#xmlcdata{ content = <<"some value">> }]
           },
    ?assertEqual(El, exml:xml_sort(El)).

sort_xmlel_test() ->
    Attrs = [{<<"attr1">>, <<"bar">>}, {<<"attr2">>, <<"baz">>}],
    El1 = #xmlel{
            name = <<"foo">>,
            attrs = Attrs,
            children = [#xmlcdata{ content = <<"some value">> }]
           },
    El2 = El1#xmlel{ attrs = lists:reverse(Attrs) },
    ?assertNotEqual(El1, El2),
    ?assertEqual(exml:xml_sort(El1), exml:xml_sort(El2)).

sort_xmlel_nested_test() ->
    Attrs = [{<<"attr1">>, <<"bar">>}, {<<"attr2">>, <<"baz">>}],
    CData = [#xmlcdata{ content = <<"some value">> }],
    Nested1 = #xmlel{
                 name = <<"n1">>,
                 attrs = Attrs,
                 children = CData
                },
    Nested2 = #xmlel{
                 name = <<"n2">>,
                 attrs = lists:reverse(Attrs),
                 children = CData
                },
    Children = [Nested1, Nested2],
    El1 = #xmlel{
             name = <<"foo">>,
             children = Children
            },
    El2 = El1#xmlel{ children = lists:reverse(Children) },
    ?assertNotEqual(El1, El2),
    %% children order matters
    ?assertNotEqual(exml:xml_sort(El1), exml:xml_sort(El2)).

sort_xmlstreamstart_test() ->
    Attrs = [{<<"attr1">>, <<"bar">>}, {<<"attr2">>, <<"baz">>}],
    SS1 = #xmlstreamstart{name = <<"n1">>, attrs = Attrs},
    SS2 = SS1#xmlstreamstart{attrs = lists:reverse(Attrs)},
    ?assertNotEqual(SS1, SS2),
    ?assertEqual(exml:xml_sort(SS1), exml:xml_sort(SS2)).

sort_xmlstreamend_test() ->
    SE1 = #xmlstreamend{name = <<"n1">>},
    SE2 = SE1,
    ?assertEqual(SE1, SE2),
    ?assertEqual(exml:xml_sort(SE1), exml:xml_sort(SE2)).

sort_xmlel_list_test() ->
    El1 = #xmlel{
            name = <<"foo">>,
            attrs = [{<<"attr1">>, <<"bar">>}],
            children = [#xmlcdata{ content = <<"some value">> }]
           },
    El2 = El1#xmlel{ name = <<"baz">> },
    L1 = [El1, El2],
    L2 = lists:reverse(L1),
    ?assertNotEqual(L1, L2),
    ?assertEqual(exml:xml_sort(L1), exml:xml_sort(L2)).

assert_xmlel_equal_macro_positive_test() ->
    Attrs = [{<<"attr1">>, <<"bar">>}, {<<"attr2">>, <<"baz">>}],
    El1 = #xmlel{
             name = <<"foo">>,
             attrs = Attrs,
             children = [#xmlcdata{ content = <<"some value">> }]
            },
    El2 = El1#xmlel{ attrs = lists:reverse(Attrs) },
    ?assertEqual(ok, ?exmlAssertEqual(El1, El2)).

assert_xmlel_equal_macro_negative_test() ->
    El1 = #xmlel{
             name = <<"foo">>,
             attrs = [{<<"attr1">>, <<"bar">>}, {<<"attr2">>, <<"baz">>}],
             children = [#xmlcdata{ content = <<"some value">> }]
            },
    El2 = El1#xmlel{ attrs = [] },
    ?assertError({exmlAssertEqual, [_, _, _, {expected, El1}, {value, El2}]},
                 ?exmlAssertEqual(El1, El2)).

throws_error_when_record_is_invalid_test() ->
    BadExml = #xmlel{name = <<"pp">>, attrs = 1},
    ?assertError({badxml, BadExml, _}, exml:to_binary(BadExml)).

to_binary_arbitrary_stream_elements_test() ->
    Elements = [#xmlcdata{content = <<"content">>},
                #xmlstreamend{name = <<"endname">>},
                #xmlstreamstart{name = <<"name">>, attrs = [{<<"a">>, <<"b">>}]}],
    ?assertEqual(<<"content</endname><name a='b'>">>, exml:to_binary(Elements)).

parse(Doc) ->
    case exml:parse(Doc) of
        {ok, X} -> X;
        {error, E} -> throw(E)
    end.
