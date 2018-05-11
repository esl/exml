%%%-------------------------------------------------------------------
%%% @author Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc
%%%
%%% @end
%%% Created : 12 Jul 2011 by Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(exml).

-include("exml_stream.hrl").

-export([parse/1]).

-export([to_list/1,
         to_binary/1,
         to_iolist/1,
         xml_size/1,
         to_pretty_iolist/1]).

-export([]).

-export_type([attr/0,
              cdata/0,
              element/0,
              item/0]).

-type attr() :: {binary(), binary()}.
-type cdata() :: #xmlcdata{}.
-type element() :: #xmlel{}.
-type item() :: element() | attr() | cdata() | exml_stream:start() | exml_stream:stop().

-spec xml_size(item() | [item()]) -> non_neg_integer().
xml_size([]) ->
    0;
xml_size([Elem | Rest]) ->
    xml_size(Elem) + xml_size(Rest);
xml_size(#xmlcdata{ content = Content }) ->
    iolist_size(exml_nif:escape_cdata(Content));
xml_size(#xmlel{ name = Name, attrs = Attrs, children = [] }) ->
    3 % Self-closing: </>
    + byte_size(Name) + xml_size(Attrs);
xml_size(#xmlel{ name = Name, attrs = Attrs, children = Children }) ->
    % Opening and closing: <></>
    5 + byte_size(Name)*2
    + xml_size(Attrs) + xml_size(Children);
xml_size(#xmlstreamstart{ name = Name, attrs = Attrs }) ->
    byte_size(Name) + 2 + xml_size(Attrs);
xml_size(#xmlstreamend{ name = Name }) ->
    byte_size(Name) + 3;
xml_size({Key, Value}) ->
    byte_size(Key)
    + 4 % ="" and whitespace before
    + byte_size(Value).

-spec to_list(element() | [exml_stream:element()]) -> string().
to_list(Element) ->
    binary_to_list(to_binary(Element)).

-spec to_binary(element() | [exml_stream:element()]) -> binary().
to_binary(Element) ->
    to_binary(Element, not_pretty).

-spec to_iolist(element() | [exml_stream:element()]) -> iodata().
to_iolist(Element) ->
    to_binary(Element, not_pretty).

-spec to_pretty_iolist(element() | [exml_stream:element()]) -> iodata().
to_pretty_iolist(Element) ->
    to_binary(Element, pretty).

-spec parse(binary() | [binary()]) -> {ok, exml:element()} | {error, any()}.
parse(XML) ->
    exml_nif:parse(XML).

-spec stream_to_xmlel([exml_stream:element()]) -> element().
stream_to_xmlel([#xmlstreamstart{name = StartTag, attrs = Attrs} | [_ | _] = Rest]) ->
    RRest = lists:reverse(Rest),
    case hd(RRest) of
        #xmlstreamend{name = StartTag} -> ok;
        Other ->
            Reason = io_lib:format("expected #xmlstreamend{name = ~s}", [StartTag]),
            error({badxml, Other, Reason})
    end,
    Children = lists:reverse(tl(RRest)),
    #xmlel{name = StartTag, attrs = Attrs, children = Children}.

-spec to_binary(element() | [exml_stream:element()], pretty | term()) -> binary().
to_binary([_|_] = Elements, Pretty) ->
    to_binary(stream_to_xmlel(Elements), Pretty);
to_binary(#xmlel{} = Element, Pretty) ->
    case catch exml_nif:to_binary(Element, Pretty) of
        {'EXIT', Reason} -> erlang:error({badxml, Element, Reason});
        Result -> Result
    end.
