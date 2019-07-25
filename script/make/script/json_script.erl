%%%-------------------------------------------------------------------
%%% @doc
%%% module json script
%%% @end
%%%-------------------------------------------------------------------
-module(json_script).
-export([main/1]).
%% ------------------------ user guide -------------------------------
%%
%% sql      :: auto group by key(when key reduplicated)
%%
%% string type term guide
%% varchar                                   => term
%% varchar with default(<<>>) in comment     => ""
%% char                                      => ""
%% text                                      => ""
%%
%%%===================================================================
%%% API
%%%===================================================================
main([Key]) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    List = [X || X <- json(), filename:basename(element(1, X), ".js") == Key],
    console:stacktrace(catch maker:start(fun json_maker:parse/2, List)),
    ok;
main(_) ->
    io:format("invail argument~n").

%%%===================================================================
%%% base data
%%%===================================================================
json() ->
    [
        {"src/module/fashion/beauty_fashion_data.js",
            [
                {"SELECT {*} FROM `beauty_fashion_data` where `fashion_id` = 'FashionId'", "fashion"},
                {"SELECT [{*}] FROM `beauty_fashion_data` where `beauty_id` = 'BeautyId'", "beauty_fashion_list"},
                {"SELECT {*} FROM `beauty_fashion_data` where `beauty_id` = 'BeautyId' and `fashion_id` = 'FashionId' and `quality` = 'Quality'", "beauty_fashion"}
            ]
        }
    ].
