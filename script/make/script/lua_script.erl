%%%-------------------------------------------------------------------
%%! +pc unicode
%%% @doc
%%% lua script for lua maker
%%% @end
%%%-------------------------------------------------------------------
-module(lua_script).
-export([main/1]).
-include("../../../include/journal.hrl").
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
    io:setopts([{encoding, unicode}]),
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    List = [X || X <- lua(), filename:basename(element(1, X), ".lua") == Key orelse filename:basename(element(1, X), ".lua") == Key ++ "_data"],
    try
        io:format("~tp~n", [lua_maker:start(List)])
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?HALT(Class, Reason, Stacktrace)
    end;
main([]) ->
    io:format("[~n~ts~n]~n", [string:join([io_lib:format("{\"file\":\"~s\",\"description\":\"~ts\"}", [filename:basename(element(1, F)), binary_to_list(unicode:characters_to_binary(element(2, F)))]) || F <- lua()], ",\n")]);
main(Args) ->
    io:format(standard_error, "invalid argument: ~tp~n", [Args]).

%%%===================================================================
%%% lua data
%%%===================================================================
lua() ->
    [
        {"src/module/text/test_data.lua", "测试配置",
            [
                %% key -> value
                {"SELECT `en` FROM `text_data` WHERE `key` = Key", "en"},
                %% key -> column value
                {"SELECT {*} FROM `text_data` WHERE `key` = Key", "text"},
                %% key, key -> value
                {"SELECT `zhCN` FROM `error_text_data` WHERE `key` = Key AND `type` = Type ", "zhCN"},
                %% key -> [value]
                {"SELECT ALL `monster_id` FROM `monster_data` WHERE `type` = Type ", "type"},
                %% -> [value] (not unique)
                {"SELECT ALL `level` FROM `level_data` ORDER BY `level` ASC", "level"},
                %% -> [value] (unique)
                {"SELECT ALL `type` FROM `monster_data` GROUP BY `type` ", "type_list"},
                %% -> value
                {"SELECT MAX(`level`) FROM `level_data` ", "max_level"},
                %% -> value
                {"SELECT COUNT(`en`) FROM `text_data` ", "text_count"},
                %% key -> step range
                {"SELECT `level` FROM `level_data` WHERE Exp >= `exp` ORDER BY `exp` DESC DEFAULT 0 ", "get_level_by_exp"}
            ]
        },
        {"script/make/data/lua/parameter_data.lua", "自定义参数配置",
            [
                {"SELECT `value` FROM `parameter_data` WHERE `key` = Key", "get"}
            ]
        }
    ].
