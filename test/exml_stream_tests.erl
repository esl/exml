-module(exml_stream_tests).

-include("exml_stream.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

basic_parse_test() ->
    {ok, Parser0} = exml_stream:new_parser(),
    {ok, Parser1, Empty0} =
        exml_stream:parse(Parser0, <<"<stream:stream xmlns:stream='http://etherx.jabber.org/streams' version='1.0'">>),
    ?assertEqual([], Empty0),
    {ok, Parser2, StreamStart} =
        exml_stream:parse(Parser1, <<" to='i.am.banana.com' xml:lang='en'><auth">>),
    ?assertEqual(
       [#xmlstreamstart{name = <<"stream:stream">>,
                        attrs = [{<<"xmlns:stream">>, <<"http://etherx.jabber.org/streams">>},
                                 {<<"version">>, <<"1.0">>},
                                 {<<"to">>, <<"i.am.banana.com">>},
                                 {<<"xml:lang">>, <<"en">>}]}],
       StreamStart),
    {ok, Parser3, Auth} = exml_stream:parse(Parser2, <<" mechanism='DIGEST-MD5'/>">>),
    ?assertEqual(
       [#xmlel{name = <<"auth">>, attrs = [{<<"mechanism">>, <<"DIGEST-MD5">>}]}],
       Auth),
    {ok, Parser4, Empty1} = exml_stream:parse(Parser3, <<"<stream:features><bind xmlns='some_ns'">>),
    ?assertEqual([], Empty1),
    {ok, Parser5, Empty2} = exml_stream:parse(Parser4, <<"/><session xmlns='some_other'/>This is ">>),
    ?assertEqual([], Empty2),
    {ok, _Parser6, Features} = exml_stream:parse(Parser5, <<"some CData</stream:features>">>),
    ?assertMatch(
       [#xmlel{name = <<"stream:features">>,
                    children = [#xmlel{name = <<"bind">>,
                                            attrs = [{<<"xmlns">>, <<"some_ns">>}]},
                                #xmlel{name = <<"session">>,
                                            attrs = [{<<"xmlns">>, <<"some_other">>}]},
                                _CData]}],
       Features),
    [#xmlel{children=[_, _, CData]}] = Features,
    ?assertEqual(#xmlcdata{content = <<"This is some CData">>}, CData).

parser_errors_test() ->
    ?assertMatch({error, _}, exml:parse(<<"<notclosed_element>">>)),
    %% it is the special case, because we are wrapping binary in the following way
    %% Stream = <<"<stream>", XML/binary, "</stream>">>,
    %% to make it a non-blocking call(?)
    ?assertMatch({error, _}, exml:parse(<<"<stream>">>)).

-define(BANANA_STREAM, <<"<stream:stream xmlns:stream='something'><foo attr='bar'>I am a banana!<baz/></foo></stream:stream>">>).
-define(assertIsBanana(Elements), (fun() -> % fun instead of begin/end because we bind CData in unhygenic macro
                                           ?assertMatch([#xmlstreamstart{name = <<"stream:stream">>,
                                                                         attrs = [{<<"xmlns:stream">>, <<"something">>}]},
                                                         #xmlel{name = <<"foo">>,
                                                                     attrs = [{<<"attr">>, <<"bar">>}],
                                                                     children = [_CData, #xmlel{name = <<"baz">>}]},
                                                         #xmlstreamend{name = <<"stream:stream">>}],
                                                        Elements),
                                           [_, #xmlel{children=[CData|_]}|_] = Elements,
                                           ?assertEqual(#xmlcdata{content = <<"I am a banana!">>}, CData),
                                           Elements
                                   end)()).

conv_test() ->
    AssertParses = fun(Input) ->
                           {ok, Parser0} = exml_stream:new_parser(),
                           {ok, _Parser1, Elements} = exml_stream:parse(Parser0, Input),
                           ?assertIsBanana(Elements)
                   end,
    Elements = AssertParses(?BANANA_STREAM),
    AssertParses(exml:to_binary(Elements)),
    AssertParses(list_to_binary(exml:to_list(Elements))),
    AssertParses(exml:to_iolist(Elements)),
    AssertParses(iolist_to_binary(re:replace(exml:to_pretty_iolist(Elements), "\n\\s*", "", [global]))).

stream_reopen_test() ->
    {ok, Parser0} = exml_stream:new_parser(),
    {ok, Parser1, Elements1} = exml_stream:parse(Parser0, ?BANANA_STREAM),
    ?assertIsBanana(Elements1),
    {ok, Parser2} = exml_stream:reset_parser(Parser1),
    {ok, _Parser3, Elements2} = exml_stream:parse(Parser2, ?BANANA_STREAM),
    ?assertIsBanana(Elements2).

infinit_framed_stream_test() ->
    {ok, Parser0} = exml_stream:new_parser([{infinite_stream, true},
                                            {autoreset, true}]),
    Els = [#xmlel{name = <<"open">>,
                  attrs = [{<<"xmlns">>, <<"urn:ietf:params:xml:ns:xmpp-framing">>},
                           {<<"to">>, <<"example.com">>},
                           {<<"version">>, <<"1.0">>}]},
           #xmlel{name = <<"foo">>},
           #xmlel{name = <<"message">>,
                  attrs = [{<<"to">>, <<"ala@example.com">>}],
                  children = [#xmlel{name = <<"body">>,
                                     children = [#xmlcdata{content = <<"Hi, How Are You?">>}]}]}
    ],
    lists:foldl(fun(#xmlel{name = Name} = Elem, Parser) ->
        Bin = exml:to_binary(Elem),
        {ok, Parser1, [Element]} = exml_stream:parse(Parser, Bin), %% matches to one element list
        #xmlel{ name = Name} = Element, %% checks if returned is xmlel of given name
        Parser1
    end, Parser0, Els).

parse_error_test() ->
    {ok, Parser0} = exml_stream:new_parser(),
    Input = <<"top-level non-tag">>,
    ?assertMatch({error, _}, exml_stream:parse(Parser0, Input)).

assert_parses_escape_cdata(Text) ->
    Tag = #xmlel{name = <<"tag">>, children=[#xmlcdata{content = Text}]},
    Stream = [#xmlstreamstart{name = <<"s">>}, Tag, #xmlstreamend{name = <<"s">>}],
    {ok, Parser0} = exml_stream:new_parser(),
    {ok, _Parser1, Elements} = exml_stream:parse(Parser0, exml:to_binary(Stream)),
    ?assertMatch([#xmlstreamstart{name = <<"s">>},
                  #xmlel{name = <<"tag">>, children=[_CData]},
                  #xmlstreamend{name = <<"s">>}],
                 Elements),
    [_, #xmlel{children=[CData]}, _] = Elements,
    ?assertEqual(#xmlcdata{content = Text}, CData).

reset_parser_error_test() ->
    {ok, _P} = exml_stream:new_parser(),
    BadParser = {parser, foo, bar, baz},
    ?assertError(function_clause, exml_stream:reset_parser(BadParser)).

cdata_is_an_error_when_first_child_of_stream_test() ->
    {ok, P} = exml_stream:new_parser(),
    ?assertMatch({error, _}, exml_stream:parse(P, <<"<stream>hello</stream>">>)).

multiple_cdata_are_joined_test() ->
    {ok, P} = exml_stream:new_parser(),
    {ok, P1, _} = exml_stream:parse(P, <<"<s><a><![CDATA[hello]]>">>),
    {ok, P2, E1} = exml_stream:parse(P1, <<", world</a>">>),
    {ok, _,  _} = exml_stream:parse(P2, <<"</s>">>),
    #xmlel{children=[CData]} = hd(E1),
    ?assertEqual(#xmlcdata{content = <<"hello, world">>}, CData).

cdata_test() ->
    assert_parses_escape_cdata(<<"I am a banana!">>),
    assert_parses_escape_cdata(<<"]:-> ]]> >">>),
    assert_parses_escape_cdata(<<"><tag">>),
    assert_parses_escape_cdata(<<"<!--">>),
    assert_parses_escape_cdata(<<"<![CDATA[ test">>).

-define(ATTR_TEST_STREAM, <<"<stream:stream xmlns:stream='something'><quote attr=\"&amp;&lt;&gt;&quot;&apos;&#xA;&#x9;&#xD;\"/></stream:stream>">>).

conv_attr_test() ->
    AssertParses = fun(Input) ->
                           {ok, Parser0} = exml_stream:new_parser(),
                           {ok, _Parser1, Elements} = exml_stream:parse(Parser0, Input),
                           ?assertMatch([_, #xmlel{attrs = [{<<"attr">>, <<"&<>\"'\n\t\r">>}]} | _],
                                                   Elements),
                           Elements
                   end,
    Elements = AssertParses(?ATTR_TEST_STREAM),
    AssertParses(exml:to_binary(Elements)),
    AssertParses(list_to_binary(exml:to_list(Elements))),
    AssertParses(iolist_to_binary(exml:to_iolist(Elements))),
    AssertParses(iolist_to_binary(re:replace(exml:to_pretty_iolist(Elements), "\n\s+", "", [global]))).

-define(RESTART_TEST_STREAM, <<"<stream:stream xmlns:stream='something'><quote/><stream:stream xmlns:stream='else'><other/><things/>">>).

stream_restarts_test() ->
    {ok, Parser0} = exml_stream:new_parser([{start_tag, <<"stream:stream">>}]),
    {ok, _Parser1, Elements} = exml_stream:parse(Parser0, ?RESTART_TEST_STREAM),
    ?assertMatch(
        [#xmlstreamstart{name = <<"stream:stream">>},
         #xmlel{name = <<"quote">>},
         #xmlstreamstart{name = <<"stream:stream">>},
         #xmlel{name = <<"other">>},
         #xmlel{name = <<"things">>}],
        Elements).

-define(RESTART_TEST_STREAM_XMLDEC, <<"<stream:stream xmlns:stream='something'><quote/>"
                                      "<?xml version='1.0'?><stream:stream xmlns:stream='a'><other/>">>).

stream_restarts_with_xml_declaration_test() ->
    {ok, Parser0} = exml_stream:new_parser([{start_tag, <<"stream:stream">>}]),
    {ok, _Parser1, Elements} = exml_stream:parse(Parser0, ?RESTART_TEST_STREAM_XMLDEC),
    ?assertMatch(
        [#xmlstreamstart{name = <<"stream:stream">>},
         #xmlel{name = <<"quote">>},
         #xmlstreamstart{name = <<"stream:stream">>},
         #xmlel{name = <<"other">>}],
        Elements).

stream_max_child_size_test() ->
    {ok, Parser0} = exml_stream:new_parser([{max_child_size, 15}]),
    {ok, Parser1, _} = exml_stream:parse(Parser0, <<"<stream><root>">>),
    {ok, Parser2, _} = exml_stream:parse(Parser1, <<"<a></a>">>),
    ?assertEqual({error, "child element too big"}, exml_stream:parse(Parser2, <<"<b>456</b>">>)).
