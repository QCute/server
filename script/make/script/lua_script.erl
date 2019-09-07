%%%-------------------------------------------------------------------
%%% @doc
%%% module lua script
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
%% varchar with default(<<>>) in comment     => ""
%% char                                      => ""
%% text                                      => ""
%%
%%%===================================================================
%%% API
%%%===================================================================
main([Key]) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    List = [X || X <- lua(), filename:basename(element(1, X), ".lua") == Key],
    console:stacktrace(catch lua_maker:start(List));
main(_) ->
    io:format("invalid argument~n").

%%%===================================================================
%%% base data
%%%===================================================================
lua() ->
    [
        {"error_code_data.lua", %% 错误码配置
            [
                {"SELECT `content` FROM `error_code_data` WHERE `protocol` = 'Protocol' AND `code` = 'Code'", ""}
            ]
        },
        {"validity_data.lua", %% 数据键值校验配置
            [
                {"SELECT `value` FROM `validity_data` WHERE `type` = 'Type' AND `key` = 'Key'", "value"},
                {"SELECT [*] FROM `validity_data` WHERE `type` = 'Type'", "type"}
            ]
        }
    ].
