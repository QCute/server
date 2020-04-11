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
%%
%%%===================================================================
%%% API functions
%%%===================================================================
main([Key]) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    List = [X || X <- json(), filename:basename(element(1, X), ".js") == Key orelse filename:basename(element(1, X), ".js") == Key ++ "_data"],
    io:format("~p~n", [catch json_maker:start(List)]);
main(_) ->
    io:format("invalid argument~n").

%%%===================================================================
%%% base data
%%%===================================================================
json() ->
    [
        {"parameter_data.js", %% 自定义参数配置
            [
                {"SELECT `value` FROM `parameter_data` WHERE `key` = Key", "get"}
            ]
        }
    ].
