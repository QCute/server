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
        {"src/module/text/error_code_data.lua", %% 错误码配置
            [
                {"SELECT `text` FROM `error_code_data` WHERE `protocol` = 'Protocol' AND `code` = 'Code'", ""}
            ]
        },
        {"src/module/accost/accost_data.lua", %% 搭讪配置
            [
                {"SELECT {`num_id`, `type`, `obj_id`, `hour_start`, `hour_end`} FROM `accost_data` where `day_of_week` = 'DayOfWeek' AND `hour_start` = 'HourStart' AND `hour_end` = 'HourEnd'", ""}
            ]
        },
        {"src/module/fashion/beauty_fashion_data.lua", %% 时装配置
            [
                {"SELECT {*} FROM `beauty_fashion_data` where `fashion_id` = 'FashionId'", "fashion"},
                {"SELECT [{*}] FROM `beauty_fashion_data` where `beauty_id` = 'BeautyId'", "beauty_fashion_list"},
                {"SELECT {*} FROM `beauty_fashion_data` where `beauty_id` = 'BeautyId' and `fashion_id` = 'FashionId' and `quality` = 'Quality'", "beauty_fashion"}
            ]
        }
    ].
