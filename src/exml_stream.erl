%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2021, Erlang Solutions Ltd.
%%% @doc XML stream parser
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

-export_type([element/0,
              start/0,
              stop/0,
              parser/0,
              parser_opt/0]).

-record(parser, {
                 event_parser :: exml_nif:parser(),
                 buffer :: [binary()]
                }).

-type start() :: #xmlstreamstart{}.
%% `#xmlstreamstart{}' record.
-type stop() :: #xmlstreamend{}.
%% `#xmlstreamend{}' record.
-type parser() :: #parser{}.
%% `#parser{}' record.
-type element() :: exml_nif:stream_element().
%% One of `t:start()', `t:stop()' or `t:exml:element()'.

-type parser_opt() :: {infinite_stream, boolean()} | {max_element_size, non_neg_integer()}.
%% Parser options
%%
%% <ul>
%%  <li>`infinite_stream': No distinct `t:start()' or `t:stop()', only `#xmlel{}' will be returned.</li>
%%  <li>`max_element_size': Specifies maximum byte size of any parsed XML element.
%%      The only exception is the "stream start" element,
%%      for which only the size of the opening tag is limited.</li>
%% </ul>

%%%===================================================================
%%% Public API
%%%===================================================================

%% @see new_parser/1
-spec new_parser() -> {ok, parser()} | {error, any()}.
new_parser() ->
    new_parser([]).

%% @doc Creates a new parser
-spec new_parser([parser_opt()]) -> {ok, parser()} | {error, any()}.
new_parser(Opts)->
    MaxElementSize = proplists:get_value(max_element_size, Opts, 0),
    InfiniteStream = proplists:get_value(infinite_stream, Opts, false),
    case exml_nif:create(MaxElementSize, InfiniteStream) of
        {ok, EventParser} ->
            {ok, #parser{event_parser = EventParser, buffer = []}};
        Error ->
            Error
    end.

%% @doc Makes a parser parse input
-spec parse(parser(), binary()) ->
    {ok, parser(), [exml_stream:element()]} | {error, Reason :: any()}.
parse(Parser, Input) when is_binary(Input) ->
    #parser{event_parser = EventParser, buffer = OldBuf} = Parser,
    Buffer = OldBuf ++ [Input],
    case parse_all(EventParser, Buffer, []) of
        {ok, Elems, NewBuffer} ->
            {ok, Parser#parser{buffer = NewBuffer}, Elems};
        Other ->
            Other
    end.

%% @doc Resets the parser's buffers
-spec reset_parser(parser()) -> {ok, parser()}.
reset_parser(#parser{event_parser = NifParser} = Parser) ->
    exml_nif:reset_parser(NifParser),
    {ok, Parser#parser{buffer = []}}.

%% @doc Free a parser
%%
%% Kept for backwards-compatibility, it is a no-op.
-spec free_parser(parser()) -> ok.
free_parser(#parser{}) ->
    ok.

%%%===================================================================
%%% Helpers
%%%===================================================================

parse_all(_Parser, [], Acc) ->
    {ok, lists:reverse(Acc), []};
parse_all(Parser, Buffer, Acc) ->
    Val = exml_nif:parse_next(Parser, Buffer),
    case Val of
        {ok, undefined, Offset} ->
            {ok, lists:reverse(Acc), drop_offset(Buffer, Offset)};
        {ok, Element, Offset} ->
            parse_all(Parser, drop_offset(Buffer, Offset), [Element | Acc]);
        {error, _} = Error ->
            Error
    end.

drop_offset(Buffer, 0) ->
    Buffer;
drop_offset([Front | Rest], Offset) when byte_size(Front) =< Offset ->
    drop_offset(Rest, Offset - byte_size(Front));
drop_offset([Front | Rest], Offset) ->
    <<_:Offset/binary, Part/binary>> = Front,
    [Part | Rest].
