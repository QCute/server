%%%------------------------------------------------------------------
%%% @doc
%%% module lua script
%%% @end
%%%------------------------------------------------------------------
-module(lua_script).
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
%%%==================================================================
%%% API functions
%%%==================================================================
main([Key]) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    List = [X || X <- lua(), filename:basename(element(1, X), ".lua") == Key orelse filename:basename(element(1, X), ".lua") == Key ++ "_data"],
    io:format("~p~n", [catch lua_maker:start(List)]);
main(_) ->
    io:format("invalid argument~n").

%%%==================================================================
%%% base data
%%%==================================================================
lua() ->
    [
        {"parameter_data.lua", %% 自定义参数配置
            [
                {"SELECT `value` FROM `parameter_data` WHERE `key` = Key", "get"}
            ]
        }
    ].
