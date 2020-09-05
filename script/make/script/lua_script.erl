%%%-------------------------------------------------------------------
%%% @doc
%%% lua script for lua maker
%%% @end
%%%-------------------------------------------------------------------
-module(lua_script).
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
    List = [X || X <- lua(), filename:basename(element(1, X), ".lua") == Key orelse filename:basename(element(1, X), ".lua") == Key ++ "_data"],
    io:format("~p~n", [catch lua_maker:start(List)]);
main(_) ->
    io:format("invalid argument~n").

%%%===================================================================
%%% lua
%%%===================================================================
lua() ->
    [
        {"script/make/data/lua/parameter_data.lua", %% 自定义参数配置
            [
                {"SELECT `value` FROM `parameter_data` WHERE `key` = Key", "get"}
            ]
        },
        {"script/make/data/lua/sign_data.lua", %% 签到配置
            [
                {"SELECT `award` FROM `sign_data` WHERE `day` = Day", "get"}
            ]
        }
    ].
