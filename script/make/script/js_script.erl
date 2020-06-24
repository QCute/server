%%%-------------------------------------------------------------------
%%% @doc
%%% module js script
%%% @end
%%%-------------------------------------------------------------------
-module(js_script).
-export([main/1]).
%% ------------------------ user guide -------------------------------
%%
%% sql      :: auto group by key(when key reduplicated)
%%
%% string type term guide
%% varchar                                   => term
%% char                                      => ""
%%
%%%===================================================================
%%% API functions
%%%===================================================================
main([Key]) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    List = [X || X <- js(), filename:basename(element(1, X), ".js") == Key orelse filename:basename(element(1, X), ".js") == Key ++ "_data"],
    io:format("~p~n", [catch js_maker:start(List)]);
main(_) ->
    io:format("invalid argument~n").

%%%===================================================================
%%% base data
%%%===================================================================
js() ->
    [
        {"parameter_data.js", %% 自定义参数配置
            [
                {"SELECT `value` FROM `parameter_data` WHERE `key` = Key", "get"}
            ]
        }
    ].
