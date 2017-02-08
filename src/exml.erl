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
         to_pretty_iolist/1, to_pretty_iolist/3]).

-export([escape_attr/1,
         unescape_attr/1,
         escape_cdata/1,
         unescape_cdata/1,
         unescape_cdata_as/2]).

-on_load(load/0).

%% Maximum bytes passed to the NIF handler at once
%% Current value is erlang:system_info(context_reductions) * 10
-define(MAX_BYTES_TO_NIF, 20000).

-export_type([attr/0,
              cdata/0,
              element/0,
              item/0]).

-type attr() :: {binary(), binary()}.
-type cdata() :: #xmlcdata{}.
-type element() :: #xmlel{}.
-type item() :: element() | attr() | cdata() | exml_stream:start() | exml_stream:stop().

-spec load() -> any().
load() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, _} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    erlang:load_nif(filename:join(PrivDir, "exml_escape"), none).

-spec xml_size(item() | [item()]) -> non_neg_integer().
xml_size([]) ->
    0;
xml_size([Elem | Rest]) ->
    xml_size(Elem) + xml_size(Rest);
xml_size(#xmlcdata{ content = Content }) ->
    iolist_size(escape_cdata_nif(Content));
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

-spec to_list(item() | [item()]) -> string().
to_list(Element) ->
    binary_to_list(to_binary(Element)).

-spec to_binary(item() | [item()]) -> binary().
to_binary(Element) ->
    case catch list_to_binary(to_iolist(Element)) of
        {'EXIT', Reason} -> erlang:error({badxml, Element, Reason});
        Reasult -> Reasult
    end.

-spec to_iolist(item() | [item()]) -> iolist().
to_iolist(Elements) when is_list(Elements) ->
    lists:map(fun item_to_iolist/1, Elements);
to_iolist(Element) ->
    item_to_iolist(Element).

-spec item_to_iolist(item()) -> iolist().
item_to_iolist(#xmlel{name = Name, attrs = Attrs, children = []}) ->
    ["<", Name, attrs_to_iolist(Attrs), "/>"];
item_to_iolist(#xmlel{name = Name, attrs = Attrs, children = Children}) ->
    ["<", Name, attrs_to_iolist(Attrs), ">",
     lists:map(fun item_to_iolist/1, Children),
     "</", Name, ">"];
item_to_iolist(#xmlstreamstart{name = Name, attrs = Attrs}) ->
    ["<", Name, attrs_to_iolist(Attrs), ">"];
item_to_iolist(#xmlstreamend{name = Name}) ->
    ["</", Name, ">"];
item_to_iolist(#xmlcdata{content = Content}) ->
    [escape_cdata_nif(Content)]. %% ensure we return io*list*

-spec to_pretty_iolist(item() | [item()]) -> iolist().
to_pretty_iolist(ElementOrElements) ->
    to_pretty_iolist(ElementOrElements, 0, "  ").

-spec to_pretty_iolist(item() | [item()], non_neg_integer(), string()) -> iolist().
to_pretty_iolist(Elements, Level, Indent) when is_list(Elements) ->
    [item_to_pretty_iolist(Element, Level, Indent) || Element <- Elements];
to_pretty_iolist(Element, Level, Indent) ->
    item_to_pretty_iolist(Element, Level, Indent).

-spec item_to_pretty_iolist(item(), non_neg_integer(), string()) -> iolist().
item_to_pretty_iolist(#xmlel{name = Name, attrs = Attrs, children = []},
                      Level, Indent) ->
    Shift = lists:duplicate(Level, Indent),
    [Shift, "<", Name, attrs_to_iolist(Attrs), "/>\n"];
item_to_pretty_iolist(#xmlel{name = Name, attrs = Attrs,
                             children = [#xmlcdata{content = Content}]},
                      Level, Indent) ->
    Shift = lists:duplicate(Level, Indent),
    [Shift, "<", Name, attrs_to_iolist(Attrs), ">",
     Content, "</", Name, ">\n"];
item_to_pretty_iolist(#xmlel{name = Name, attrs = Attrs, children = Children},
                      Level, Indent) ->
    Shift = lists:duplicate(Level, Indent),
    [Shift, "<", Name, attrs_to_iolist(Attrs), ">\n",
     [item_to_pretty_iolist(C, Level+1, Indent) || C <- Children],
     Shift, "</", Name, ">\n"];
item_to_pretty_iolist(#xmlstreamstart{name = Name, attrs = Attrs}, Level, Indent) ->
    Shift = lists:duplicate(Level, Indent),
    [Shift, "<", Name, attrs_to_iolist(Attrs), ">\n"];
item_to_pretty_iolist(#xmlstreamend{name = Name}, Level, Indent) ->
    Shift = lists:duplicate(Level, Indent),
    [Shift, "</", Name, ">\n"];
item_to_pretty_iolist(#xmlcdata{content = Content}, Level, Indent) ->
    Shift = lists:duplicate(Level, Indent),
    [Shift, Content, "\n"].

-spec attrs_to_iolist([attr()]) -> iolist().
attrs_to_iolist(Attrs) ->
    lists:map(fun({Name, Value}) ->
                      [" ", Name, "='", escape_attr(Value), "'"]
              end, Attrs).

-spec parse(binary()) -> {ok, exml:element()} | {error, any()}.
parse(XML) ->
    {ok, Parser} = exml_stream:new_parser(),
    Stream = <<"<stream>", XML/binary, "</stream>">>,
    Result = case exml_stream:parse(Parser, Stream) of
                 {ok, _, [#xmlstreamstart{}, Tree, #xmlstreamend{}]} ->
                     {ok, Tree};
                 {ok, _, Other} ->
                     {error, {bad_parse, Other}};
                 {error, Error} ->
                     {error, Error}
             end,
    ok = exml_stream:free_parser(Parser),
    Result.

-spec escape_cdata(iodata()) -> cdata().
escape_cdata(Content) ->
    BContent = list_to_binary([Content]),
    NewContent = feed_nif(fun escape_cdata_nif/1, BContent,
                          byte_size(BContent), []),
    #xmlcdata{content = NewContent}.

-spec unescape_cdata(cdata()) -> binary().
unescape_cdata(#xmlcdata{content = Content}) ->
    BContent = list_to_binary([Content]),
    feed_nif(fun unescape_cdata_nif/1, BContent, byte_size(BContent), []).

-spec unescape_cdata_as(binary|list|iodata, cdata()) -> binary().
unescape_cdata_as(What, CData) ->
    unescape_cdata_as_erl(What, CData).

-spec escape_cdata_nif(iodata()) -> binary().
escape_cdata_nif(_Data) ->
    erlang:nif_error({?MODULE, nif_not_loaded}).

-spec unescape_cdata_nif(iodata()) -> binary().
unescape_cdata_nif(_Data) ->
    erlang:nif_error({?MODULE, nif_not_loaded}).

-spec unescape_cdata_as_erl(binary|list|iodata, cdata()) -> binary().
unescape_cdata_as_erl(What, #xmlcdata{content=GtEsc}) ->
    LtEsc  = re:replace(GtEsc,  "&gt;",  ">",   [global]),
    AmpEsc = re:replace(LtEsc,  "&lt;",  "<",   [global]),
    Text   = re:replace(AmpEsc, "&amp;", "\\&", [global, {return, What}]),
    Text.

-spec escape_attr(binary()) -> binary().
escape_attr(Text) ->
    feed_nif(fun escape_attr_nif/1, Text, byte_size(Text), []).

-spec unescape_attr(binary()) -> binary().
unescape_attr(Text) ->
    feed_nif(fun unescape_attr_nif/1, Text, byte_size(Text), []).

-spec feed_nif(function(), binary(), integer(), list()) -> binary().
feed_nif(Fun, Text, Size, Acc) when Size > ?MAX_BYTES_TO_NIF ->
    <<Chunk:?MAX_BYTES_TO_NIF/binary, Rest/binary>> = Text,
    Resp = Fun(Chunk),
    feed_nif(Fun, Rest, Size - ?MAX_BYTES_TO_NIF, [Resp | Acc]);
feed_nif(Fun, Text, _Size, Acc) ->
    Resp = Fun(Text),
    list_to_binary(lists:reverse([Resp | Acc])).

-spec escape_attr_nif(binary()) -> binary().
escape_attr_nif(_Data) ->
    erlang:nif_error({?MODULE, nif_not_loaded}).

-spec unescape_attr_nif(binary()) -> binary().
unescape_attr_nif(_Data) ->
    erlang:nif_error({?MODULE, nif_not_loaded}).
