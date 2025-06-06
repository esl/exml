%%%-------------------------------------------------------------------
%%% @copyright (C) 2018-2024, Erlang Solutions Ltd.
%%% @private
%%%-------------------------------------------------------------------

-module(exml_nif).

-nifs([create/2, escape_cdata/2, to_binary/2, parse/1, parse_next/2, reset_parser/1]).

-type parser() :: term().

-export([create/2, parse/1, parse_next/2, escape_cdata/2,
         to_binary/2, reset_parser/1]).

-on_load(load/0).

%%%===================================================================
%%% Public API
%%%===================================================================

-dialyzer({nowarn_function, [load/0]}).
-spec load() -> any().
load() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, _} ->
                      case code:which(?MODULE) of
                          Path when is_list(Path) ->
                              EbinDir = filename:dirname(Path),
                              AppPath = filename:dirname(EbinDir),
                              filename:join(AppPath, "priv");
                          _ ->
                              %% cover_compiled | preloaded | non_existing
                              erlang:error({cannot_get_load_path, ?MODULE})
                      end;
                  Path ->
                      Path
              end,
    erlang:load_nif(filename:join(PrivDir, ?MODULE_STRING), none).

-spec create(MaxChildSize :: non_neg_integer(), InfiniteStream :: boolean()) ->
    {ok, parser()} | {error, Reason :: any()}.
create(_, _) ->
    erlang:nif_error(not_loaded).

-spec escape_cdata(Bin :: iodata(), atom()) -> binary().
escape_cdata(_Bin, _Style) ->
     erlang:nif_error(not_loaded).

-spec to_binary(Elem :: exml:element(), pretty | not_pretty) -> binary().
to_binary(_Elem, _Pretty) ->
    erlang:nif_error(not_loaded).

-spec parse(Bin :: binary() | [binary()]) -> {ok, exml:element()} | {error, binary()}.
parse(_) ->
    erlang:nif_error(not_loaded).

-spec parse_next(parser(), Data :: binary() | [binary()]) ->
    {ok, exml_stream:element() | undefined, non_neg_integer()} |
    {error, Reason :: any()}.
parse_next(_, _) ->
    erlang:nif_error(not_loaded).

-spec reset_parser(parser()) -> any().
reset_parser(_) ->
    erlang:nif_error(not_loaded).
