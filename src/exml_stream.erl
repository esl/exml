%%%-------------------------------------------------------------------
%%% @author Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc XML stream parser
%%%
%%% @end
%%% Created : 21 Jul 2011 by Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(exml_stream).

-include("exml_stream.hrl").

-export([new_parser/0,
         new_parser/1,
         parse/2,
         reset_parser/1,
         free_parser/1]).
-export_type([parser/0]).
-export_type([parser_type/0]).

%% single_doc - the old way one document: <stream:stream> ..... </stream:stream>
%% multiple_docs - xmpp over websockets(IETF 89) replaces <stream:..> tags with
%% <open/> <close/>. As a result we have a stream of multiple xml documents instead of one document.
%% This type of parser can handle that stream.
%% see: http://datatracker.ietf.org/doc/draft-ietf-xmpp-websocket/?include_text=1
-type parser_type() :: single_doc | multiple_docs.

-record(parser, {
        event_parser,
        type,
        stack = []
}).

-type parser() :: #parser{}.

%%%===================================================================
%%% Public API
%%%===================================================================

-spec new_parser() -> {ok, #parser{}} | {error, any()}.
new_parser() ->
    new_parser(single_doc).

-spec new_parser(parser_type()) -> {ok, #parser{}} | {error, any()}.
new_parser(Type)->
    case exml_event:new_parser() of
        {ok, EventParser} ->
            prepare_parser(#parser{event_parser = EventParser, type = Type});
        {error, Error} ->
            {error, Error}
    end.

-spec parse(#parser{}, binary()) ->
        {ok, #parser{}, [xmlstreamelement()]} | {error, {string(), binary()}}.
parse(#parser{event_parser = EventParser, stack = OldStack} = Parser, Input) ->
    case exml_event:parse(EventParser, Input) of
        {ok, Events} ->
            {Elements, NewStack} = parse_events(Events, OldStack, []),
            {ok, Parser#parser{stack=NewStack}, Elements};
        {error, Msg} ->
            {error, {Msg, Input}}
    end.

-spec reset_parser(#parser{}) -> {ok, #parser{}} | {error, any()}.
reset_parser(#parser{event_parser=EventParser}=Parser) ->
    case exml_event:reset_parser(EventParser) of
        ok ->
            %% drop all the state except event_parser
            prepare_parser(#parser{event_parser = EventParser, type = Parser#parser.type});
        Error ->
            {error, Error}
    end.

-spec free_parser(#parser{}) -> ok | {error, any()}.
free_parser(#parser{event_parser = EventParser}) ->
    exml_event:free_parser(EventParser).

%%%===================================================================
%%% Helpers
%%%===================================================================
-spec prepare_parser(#parser{}) -> {ok, #parser{}}.
prepare_parser(#parser{type = single_doc} = Parser) ->
    {ok,Parser};
prepare_parser(#parser{type = multiple_docs} = Parser) ->
    %% open fake stream tag and discard events
    %% use random bytes to reduce probability of closing the tag by user
    <<I:128/integer>> = crypto:rand_bytes(16),
    Tag = list_to_binary(erlang:integer_to_list(I,16)),
    {ok, NewParser, _ } = parse(Parser, <<"<_",Tag/binary,">">>),
    {ok, NewParser}.

-spec parse_events(list(), list(), list()) -> {list(xmlstreamelement()), list()}.
parse_events([], Stack, Acc) ->
    {lists:reverse(Acc), Stack};
parse_events([{xml_element_start, Name, NSs, Attrs} | Rest], [], Acc) ->
    NewAttrs = nss_to_fake_attrs(NSs, []) ++ Attrs,
    parse_events(Rest, [#xmlel{name = Name, attrs = NewAttrs}],
                 [#xmlstreamstart{name = Name, attrs = NewAttrs} | Acc]);
parse_events([{xml_element_start, Name, NSs, Attrs} | Rest], Stack, Acc) ->
    NewAttrs = nss_to_fake_attrs(NSs, []) ++ Attrs,
    parse_events(Rest, [#xmlel{name = Name, attrs = NewAttrs} | Stack], Acc);
parse_events([{xml_element_end, Name} | Rest], [#xmlel{name = Name}], Acc) ->
    parse_events(Rest, [], [#xmlstreamend{name = Name} | Acc]);
parse_events([{xml_element_end, Name} | Rest], [#xmlel{name = Name} = Element, Top], Acc) ->
    parse_events(Rest, [Top], [xml_element(Element) | Acc]);
parse_events([{xml_element_end, _Name} | Rest], [Element, Parent | Stack], Acc) ->
    NewElement = Element#xmlel{children = lists:reverse(Element#xmlel.children)},
    NewParent = Parent#xmlel{children = [NewElement | Parent#xmlel.children]},
    parse_events(Rest, [NewParent | Stack], Acc);
parse_events([{xml_cdata, _CData} | Rest], [Top], Acc) ->
    parse_events(Rest, [Top], Acc);
parse_events([{xml_cdata, CData} | Rest], [#xmlel{children = [#xmlcdata{content = Content} |
                                                                   RestChildren]} = XML | Stack], Acc) ->
    NewChildren = [#xmlcdata{content = list_to_binary([Content, CData])} | RestChildren],
    parse_events(Rest, [XML#xmlel{children = NewChildren} | Stack], Acc);
parse_events([{xml_cdata, CData} | Rest], [Element | Stack], Acc) ->
    NewChildren = [#xmlcdata{content = CData} | Element#xmlel.children],
    parse_events(Rest, [Element#xmlel{children = NewChildren} | Stack], Acc).

-spec xml_element(#xmlel{}) -> #xmlel{}.
xml_element(#xmlel{children = Children} = Element) ->
    Element#xmlel{children = xml_children(Children, [])}.

-spec xml_children(list(xmlterm()), list(xmlterm())) -> list(xmlterm()).
xml_children([], Children) ->
    Children;
xml_children([#xmlcdata{content = Content1}, #xmlcdata{content = Content2} | Rest], Children) ->
    xml_children([#xmlcdata{content = list_to_binary([Content2, Content1])} | Rest], Children);
xml_children([Element | Rest], Children) ->
    xml_children(Rest, [Element | Children]).

-spec nss_to_fake_attrs([{binary(), binary() | none}], [{binary(), binary()}]) ->
        [{binary(), binary()}].
nss_to_fake_attrs([{Uri, none} | Rest], Acc) ->
    nss_to_fake_attrs(Rest, [{<<"xmlns">>, Uri} | Acc]);
nss_to_fake_attrs([{Uri, Prefix} | Rest], Acc) ->
    nss_to_fake_attrs(Rest, [{<<"xmlns:", Prefix/binary>>, Uri} | Acc]);
nss_to_fake_attrs([], Acc) ->
    Acc. %% no lists:reverse, as we got the argument list in reversed order
