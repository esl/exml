%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2024, Erlang Solutions Ltd.
%%% @doc
%%% @end
%%% Created : 12 Jul 2011 by Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%%
%%% Parts of this file, explicitly marked in the code, were taken from
%%% https://github.com/erszcz/rxml
%%%-------------------------------------------------------------------
-module(exml).

-include("exml_stream.hrl").

-export([parse/1]).

-export([to_list/1,
         to_binary/1,
         to_iolist/1,
         xml_size/1,
         xml_sort/1,
         to_pretty_iolist/1]).

-export_type([attr/0,
              cdata/0,
              element/0,
              item/0]).

-type attr() :: {binary(), binary()}.
-type cdata() :: #xmlcdata{}.
-type element() :: #xmlel{}.
-type item() :: element() | attr() | cdata() | exml_stream:start() | exml_stream:stop().
-type prettify() :: pretty | not_pretty.

%% @doc Calculate the length of the original XML payload
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

%% @doc Sort in ascending order a list of xml `t:item()'.
%%
%% Sorting is defined as calling `lists:sort/1' at:
%% <ul>
%%  <li>all the `xmlel's provided (if there is a list of them) AND</li>
%%  <li>all the `xmlel' elements' attributes recursively (the root and descendants) AND</li>
%%  <li>all the `xmlel' children recursively (the root and descendants).</li>
%% </ul>
%% @end
%% The implementation of this function is a subtle modification of
%% https://github.com/erszcz/rxml/commit/e8483408663f0bc2af7896e786c1cdea2e86e43d
-spec xml_sort(item() | [item()]) -> item() | [item()].
xml_sort(#xmlcdata{} = Cdata) ->
    Cdata;
xml_sort(#xmlel{ attrs = Attrs, children = Children } = El) ->
    El#xmlel{
      attrs = lists:sort(Attrs),
      children = [ xml_sort(C) || C <- Children ]
     };
xml_sort(#xmlstreamstart{ attrs = Attrs } = StreamStart) ->
    StreamStart#xmlstreamstart{ attrs = lists:sort(Attrs) };
xml_sort(#xmlstreamend{} = StreamEnd) ->
    StreamEnd;
xml_sort(Elements) when is_list(Elements) ->
    lists:sort([ xml_sort(E) || E <- Elements ]).

%% @equiv erlang:binary_to_list(to_binary(Element))
-spec to_list(exml_stream:element() | [exml_stream:element()]) -> string().
to_list(Element) ->
    binary_to_list(to_binary(Element)).

%% @equiv erlang:iolist_to_binary(to_iolist(Element, not_pretty))
-spec to_binary(exml_stream:element() | [exml_stream:element()]) -> binary().
to_binary(Element) ->
    iolist_to_binary(to_iolist(Element, not_pretty)).

%% @equiv to_iolist(Element, not_pretty)
-spec to_iolist(exml_stream:element() | [exml_stream:element()]) -> iodata().
to_iolist(Element) ->
    to_iolist(Element, not_pretty).

%% @equiv to_iolist(Element, pretty)
-spec to_pretty_iolist(exml_stream:element() | [exml_stream:element()]) -> iodata().
to_pretty_iolist(Element) ->
    to_iolist(Element, pretty).

%% @doc Parses a binary or a list of binaries into an XML `t:element()'.
-spec parse(binary() | [binary()]) -> {ok, exml:element()} | {error, any()}.
parse(XML) ->
    exml_nif:parse(XML).

%% @doc Turn a –list of– exml elements into iodata for IO interactions.
%%
%% The `Pretty' argument indicates if the generated XML should have new lines and indentation,
%% which is useful for the debugging eye, or should rather be a minified version,
%% which is better for IO performance.
-spec to_iolist(cdata() | exml_stream:element() | [exml_stream:element()], prettify()) -> iodata().
to_iolist(#xmlel{} = Element, Pretty) ->
    to_binary_nif(Element, Pretty);
to_iolist(#xmlstreamstart{name = Name, attrs = Attrs}, _Pretty) ->
    Result = to_binary_nif(#xmlel{name = Name, attrs = Attrs}, not_pretty),
    FrontSize = byte_size(Result) - 2,
    <<Front:FrontSize/binary, "/>">> = Result,
    [Front, $>];
to_iolist(#xmlstreamend{name = Name}, _Pretty) ->
    [<<"</">>, Name, <<">">>];
to_iolist(#xmlcdata{content = Content}, _Pretty) ->
    exml_nif:escape_cdata(Content);
to_iolist([Element], Pretty) ->
    to_iolist(Element, Pretty);
to_iolist([#xmlstreamstart{name = Name, attrs = Attrs} | Tail] = Elements, Pretty) ->
    [Last | RevChildren] = lists:reverse(Tail),
    case Last of
        #xmlstreamend{name = Name} ->
            %% Add extra nesting for streams so pretty-printing would be indented properly
            Element = #xmlel{name = Name, attrs = Attrs, children = lists:reverse(RevChildren)},
            to_binary_nif(Element, Pretty);
        _ ->
            [to_iolist(El, Pretty) || El <- Elements]
    end;
to_iolist(Elements, Pretty) when is_list(Elements) ->
    [to_iolist(El, Pretty) || El <- Elements].

-spec to_binary_nif(element(), prettify()) -> binary().
to_binary_nif(#xmlel{} = Element, Pretty) ->
    case catch exml_nif:to_binary(Element, Pretty) of
        {'EXIT', Reason} -> erlang:error({badxml, Element, Reason});
        Result when is_binary(Result) -> Result
    end.
