%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2021, Erlang Solutions Ltd.
%%% @doc Easy navigation in XML trees
%%% @end
%%%-------------------------------------------------------------------
-module(exml_query).

-include("exml.hrl").

-export([path/2, path/3]).
-export([paths/2]).
-export([subelement/2, subelement/3]).
-export([subelement_with_ns/2, subelement_with_ns/3]).
-export([subelement_with_attr/3, subelement_with_attr/4]).
-export([subelement_with_name_and_ns/3, subelement_with_name_and_ns/4]).
-export([subelements/2]).
-export([subelements_with_ns/2]).
-export([subelements_with_name_and_ns/3]).
-export([subelements_with_attr/3]).
-export([attr/2, attr/3]).
-export([cdata/1]).

-type element_with_ns() :: {element_with_ns, binary()}.
-type element_with_name_and_ns () :: {element_with_ns, binary(), binary()}.
-type element_with_attr_of_value () :: {element_with_attr, binary(), binary()}.

-type path() :: [cdata | %% selects cdata from the element
                 {element, binary()} | % selects subelement with given name
                 {attr, binary()} | % selects attr of given name
                 element_with_ns() | % selects subelement with given namespace
                 element_with_name_and_ns() | % selects subelement with given name and namespace
                 element_with_attr_of_value() % selects subelement with given attribute and value
                ].

-export_type([path/0]).

%% @doc Gets the element/attr/cdata contained in the leftmost path
%% Find an element in the xml tree by a path
%% that is pattern-matched against such xml tree structure
%%
%% For example, given an xml document like
%% ```
%% <message from='alice@localhost' to='alice@localhost/res1' id='id-1'>
%%   <result xmlns='urn:xmpp:mam:2' id='BGCH2R2950G1'>
%%     <forwarded xmlns='urn:xmpp:forward:0'>
%%       <delay xmlns='urn:xmpp:delay' stamp='2021-05-05T08:36:19Z' from='bob@localhost/res1'/>
%%       <message from='bob@localhost/res1' xmlns='jabber:client' xml:lang='en' to='alice@localhost/res1' type='chat'>
%%         <body>Message from bob to alice</body>
%%       </message>
%%     </forwarded>
%%   </result>
%% </message>
%% '''
%% The path
%% ```
%%   [{element_with_ns, <<"result">>, <<"urn:xmpp:mam:2">>},
%%    {element_with_ns, <<"forwarded">>, <<"urn:xmpp:forward:0">>},
%%    {element_with_ns, <<"message">>, <<"jabber:client">>},
%%    {element, <<"body">>},
%%    cdata}],
%% '''
%% would return `<<"Message from bob to alice">>'
%% @end
-spec path(exml:element(), path()) -> exml:element() | binary() | undefined.
path(Element, Path) ->
    path(Element, Path, undefined).

%% @doc Gets the element/attr/cdata in the leftmost possible described path.
%% Like `path/2' but returns the given `Default' if no element matches the path.
%%% @see path/2
-spec path(exml:element(), path(), Other) -> exml:element() | binary() | Other.
path(#xmlel{} = Element, [], _) ->
    Element;
path(#xmlel{} = Element, [{element, Name} | Rest], Default) ->
    Child = subelement(Element, Name), % may return undefined
    path(Child, Rest, Default);
path(#xmlel{} = Element, [{element_with_ns, NS} | Rest], Default) ->
    Child = subelement_with_ns(Element, NS),
    path(Child, Rest, Default);
path(#xmlel{} = Element, [{element_with_ns, Name, NS} | Rest], Default) ->
    Child = subelement_with_name_and_ns(Element, Name, NS),
    path(Child, Rest, Default);
path(#xmlel{} = Element, [{element_with_attr, Name, Value} | Rest], Default) ->
    Child = subelement_with_attr(Element, Name, Value),
    path(Child, Rest, Default);
path(#xmlel{} = Element, [cdata], _) ->
    cdata(Element);
path(#xmlel{} = Element, [{attr, Name}], Default) ->
    attr(Element, Name, Default);
path(_, _, Default) ->
    Default.

%% @doc gets the elements/attrs/cdatas reachable by the described path
%% @see path/2
-spec paths(exml:element(), path()) -> [exml:element() | binary()].
paths(#xmlel{} = Element, []) ->
    [Element];
paths(#xmlel{} = Element, [{element, Name} | Rest]) ->
    Children = subelements(Element, Name),
    lists:append([paths(Child, Rest) || Child <- Children]);
paths(#xmlel{} = Element, [{element_with_ns, NS} | Rest]) ->
    Children = subelements_with_ns(Element, NS),
    lists:append([paths(Child, Rest) || Child <- Children]);
paths(#xmlel{} = Element, [{element_with_ns, Name, NS} | Rest]) ->
    Children = subelements_with_name_and_ns(Element, Name, NS),
    lists:append([paths(Child, Rest) || Child <- Children]);
paths(#xmlel{} = Element, [{element_with_attr, AttrName, Value} | Rest]) ->
    Children = subelements_with_attr(Element, AttrName, Value),
    lists:append([paths(Child, Rest) || Child <- Children]);
paths(#xmlel{} = Element, [cdata]) ->
    [cdata(Element)];
paths(#xmlel{attrs = Attrs}, [{attr, Name}]) ->
    lists:sublist([V || {N, V} <- Attrs, N =:= Name], 1);
paths(#xmlel{} = El, Path) when is_list(Path) ->
    erlang:error(invalid_path, [El, Path]).

-spec subelement(exml:element(), binary()) -> exml:element() | undefined.
subelement(Element, Name) ->
    subelement(Element, Name, undefined).

-spec subelement(exml:element(), binary(), Other) -> exml:element() | Other.
subelement(#xmlel{children = Children}, Name, Default) ->
    case lists:keyfind(Name, #xmlel.name, Children) of
        false ->
            Default;
        Result ->
            Result
    end.

-spec subelement_with_ns(exml:element(), binary()) -> exml:element() | undefined.
subelement_with_ns(Element, NS) ->
    subelement_with_ns(Element, NS, undefined).

-spec subelement_with_ns(exml:element(), binary(), Other) -> exml:element() | Other.
subelement_with_ns(#xmlel{children = Children}, NS, Default) ->
    child_with_ns(Children, NS, Default).

child_with_ns([], _, Default) ->
    Default;
child_with_ns([#xmlel{} = Element | Rest], NS, Default) ->
    case attr(Element, <<"xmlns">>) of
        NS ->
            Element;
        _ ->
            child_with_ns(Rest, NS, Default)
    end;
child_with_ns([_ | Rest], NS, Default) ->
    child_with_ns(Rest, NS, Default).

-spec subelement_with_attr(exml:element(), AttrName :: binary(), AttrValue :: binary()) ->
    exml:element() | undefined.
subelement_with_attr(Element, AttrName, AttrValue) ->
    subelement_with_attr(Element, AttrName, AttrValue, undefined).

-spec subelement_with_attr(Element, AttrName, AttrValue, Other) -> SubElement | Other when
      Element :: exml:element(),
      AttrName :: binary(),
      AttrValue :: binary(),
      SubElement :: exml:element(),
      Other :: term().
subelement_with_attr(#xmlel{children = Children}, AttrName, AttrValue, Default) ->
    child_with_attr(Children, AttrName, AttrValue, Default).

child_with_attr([], _, _, Default) ->
    Default;
child_with_attr([#xmlel{} = Element | Rest], AttrName, AttrVal, Default) ->
    case attr(Element, AttrName) of
        AttrVal ->
            Element;
        _ ->
            child_with_attr(Rest, AttrName, AttrVal, Default)
    end;
child_with_attr([_ | Rest], AttrName, AttrVal, Default) ->
    child_with_attr(Rest, AttrName, AttrVal, Default).


-spec subelement_with_name_and_ns(exml:element(), binary(), binary()) ->
    exml:element() | undefined.
subelement_with_name_and_ns(Element, Name, NS) ->
    subelement_with_name_and_ns(Element, Name, NS, undefined).

-spec subelement_with_name_and_ns(exml:element(), binary(), binary(), Other) ->
    exml:element() | Other.
subelement_with_name_and_ns(Element, Name, NS, Default) ->
    case subelements_with_name_and_ns(Element, Name, NS) of
        [] ->
            Default;
        [FirstElem | _] ->
            FirstElem
    end.

-spec subelements(exml:element(), binary()) -> [exml:element()].
subelements(#xmlel{children = Children}, Name) ->
    lists:filter(fun(#xmlel{name = N}) when N =:= Name ->
                        true;
                    (_) ->
                        false
                 end, Children).

-spec subelements_with_ns(exml:element(), binary()) -> [exml:element()].
subelements_with_ns(#xmlel{children = Children}, NS) ->
    lists:filter(fun(#xmlel{} = Child) ->
                        NS =:= attr(Child, <<"xmlns">>);
                    (_) ->
                        false
                 end, Children).

-spec subelements_with_name_and_ns(exml:element(), binary(), binary()) -> [exml:element()].
subelements_with_name_and_ns(#xmlel{children = Children}, Name, NS) ->
    lists:filter(fun(#xmlel{name = SubName} = Child) ->
                         SubName =:= Name andalso
                         NS =:= attr(Child, <<"xmlns">>);
                    (_) ->
                        false
                 end, Children).

-spec subelements_with_attr(exml:element(), binary(), binary()) -> [exml:element()].
subelements_with_attr(#xmlel{children = Children}, AttrName, Value) ->
    lists:filter(fun(#xmlel{} = Child) ->
                        Value =:= attr(Child, AttrName);
                    (_) ->
                        false
                 end, Children).

-spec cdata(exml:element()) -> binary().
cdata(#xmlel{children = Children}) ->
    list_to_binary([C || #xmlcdata{content = C} <- Children]).

%% @doc Query attribute value by name.
%% Returns the attribute value associated with `Name' if `Element' contains such attribute.
%% Otherwise returns `undefined'
-spec attr(exml:element(), binary()) -> binary() | undefined.
attr(Element, Name) ->
    attr(Element, Name, undefined).

%% @doc Query attribute value by name.
%% Returns the attribute value associated with `Name' if `Element' contains such attribute.
%% Otherwise returns `Default'
%% @see attr/2
-spec attr(exml:element(), binary(), Other) -> binary() | Other.
attr(#xmlel{attrs = Attrs}, Name, Default) ->
    case lists:keyfind(Name, 1, Attrs) of
        {Name, Value} ->
            Value;
        false ->
            Default
    end.
