%%%-------------------------------------------------------------------
%%% Parts of this file, explicitly marked in the code, were taken from
%%% https://github.com/erszcz/rxml
%%%-------------------------------------------------------------------

-ifndef(EXML_HEADER).
-define(EXML_HEADER, true).

-record(xmlcdata, {content = [] :: iodata(),
                   style = escaped :: escaped | cdata}).

-record(xmlel, {name :: binary(),
                attrs = #{} :: exml:attrs(),
                children = [] :: [exml:child()]}).

%% Implementation of the exmlAssertEqual/2 macro is a modification of
%% https://github.com/erszcz/rxml/commit/e8483408663f0bc2af7896e786c1cdea2e86e43d#diff-2cb5d18741df32f4ead70c21fdd221d1
%% See assertEqual in $ERLANG/lib/stdlib-2.6/include/assert.hrl for the original.
-define(exmlAssertEqual(Expect, Expr),
        begin
        ((fun () ->
            X__X = (exml:xml_sort(Expect)),
            case (exml:xml_sort(Expr)) of
                X__X -> ok;
                X__V -> erlang:error({exmlAssertEqual,
                                     [{module, ?MODULE},
                                      {line, ?LINE},
                                      {expression, (??Expr)},
                                      {expected, Expect},
                                      {value, X__V}]})
            end
          end)())
        end).

-endif.
