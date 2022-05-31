-module(exml_properties_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml.hrl").
-compile(export_all).

p(Name, Property) ->
    ?assert(proper:quickcheck
              (proper:conjunction([{Name, Property}]),
               [100, long_result, {to_file, user}])).

vector_1_forbidden_control_char_test() ->
    ?assertMatch({error, _}, exml:parse(<<"<body>", 16#1B,"</body>">>)).

vector_2_forbidden_control_char_test() ->
    ?assertMatch({error, _}, exml:parse(<<"<body", 16#1B,"></body", 16#1B,">">>)).

vector_3_forbidden_control_char_test() ->
    ?assertMatch({error, _}, exml:parse(<<"<body lang='en' bad='", 16#1B, "'></body>">>)).

vector_4_forbidden_control_char_test() ->
    ?assertMatch({error, _},
                 exml:parse(<<"<message to='alice@localhost'><body>&amp;lt;body&amp;gt;", 16#1B,"&amp;lt;/body&amp;gt;</body></body>">>)).

fail_forbidden_control_char_test() ->
    p("All valid xml cdata can be parsed",
      ?FORALL(Doc, utf8_doc_bad(),
              not is_parseable(Doc))).

parse_test() ->
    p("All valid xml cdata can be parsed",
      ?FORALL(Doc, utf8_doc(),
              is_parseable(Doc))).

serialize_test() ->
    p("All valid xml cdata can be serialized",
      ?FORALL(Doc, utf8_doc(),
              is_binary(exml:to_binary(parse(Doc))))).

inverse_test() ->
    p("exml:parse can parse the output of exml:to_binary",
      ?FORALL(Doc, utf8_doc(),
              ok == element(1, exml:parse(exml:to_binary(parse(Doc)))))).

size_test() ->
    p("exml:size equals actual size of output xml string",
      ?FORALL(Doc, utf8_doc(),
              iolist_size(exml:to_binary(parse(Doc))) == exml:xml_size(parse(Doc)))).

is_parseable(Doc) ->
    case exml:parse(Doc) of
        {ok, _} -> true;
        _ -> false
    end.

parse(Doc) ->
    case exml:parse(Doc) of
        {ok, X} -> X;
        {error, E} -> throw(E)
    end.

%%
%%  Generators
%%

utf8_doc() ->
    ?LET({{ElOpen,ElClose}, Cdata},
         {xml_open_close(), xml_cdata()},
         unicode:characters_to_binary(ElOpen ++ Cdata ++ ElClose)).

utf8_doc_bad() ->
    ?LET({{ElOpen,ElClose}, Cdata},
         {xml_open_close_maybe_bad(), utf8_text_bad()},
         unicode:characters_to_binary(ElOpen ++ Cdata ++ ElClose)).

xml_open_close() ->
    ?LET(TagName, tagname_text(),
         {lists:flatten("<" ++ TagName ++ ">"),
          lists:flatten("</" ++ TagName ++ ">")}).

xml_open_close_maybe_bad() ->
    ?LET(TagName, tagname_text_maybe_bad(),
         {lists:flatten("<" ++ TagName ++ ">"),
          lists:flatten("</" ++ TagName ++ ">")}).

tagname_text() ->
    non_empty(list(choose($a, $z))).

tagname_text_maybe_bad() ->
    non_empty(list(oneof([$a, $z, xml_c0_forbidden_control()]))).

%% see: https://en.wikipedia.org/wiki/Valid_characters_in_XML#XML_1.0
utf8_char() ->
    oneof([xml_escaped_entity(),
           xml_c0_control(),
           xml_utf8_bmp_char()]).

xml_c0_control() ->
    elements([16#0009, 16#000A, 16#000D]).

xml_c0_forbidden_control() ->
    elements([16#0000, 16#0001, 16#0002, 16#0003, 16#0004, 16#0005, 16#0006, 16#0007,
              16#0008,                   16#000B, 16#000C,          16#000E, 16#000F,
              16#0010, 16#0011, 16#0012, 16#0013, 16#0014, 16#0015, 16#0016, 16#0017,
              16#0018, 16#0019, 16#001A, 16#001B, 16#001C, 16#001D, 16#001E, 16#001F]).

utf8_text_bad() ->
    non_empty(list(xml_c0_forbidden_control())).

xml_utf8_bmp_char() ->
    ?SUCHTHAT(C, oneof([choose(16#0020,16#D7FF),
                        choose(16#E000, 16#FFFD)]),
              not lists:member(C, [$<,$>,$&])).

xml_escaped_entity() ->
    oneof(["&amp;", "&lt;", "&gt;"]).

utf8_text() ->
    non_empty(list(utf8_char())).

xml_cdata() ->
    utf8_text().
