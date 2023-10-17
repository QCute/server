%%%-------------------------------------------------------------------
%%! +pc unicode
%%% @doc
%%% lua script for lua maker
%%% @end
%%%-------------------------------------------------------------------
-module(lua_script).
-export([main/1]).
-include("../../../include/journal.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
main([]) ->
    io:setopts([{encoding, unicode}]),
    io:setopts(standard_error, [{encoding, unicode}]),
    List = [io_lib:format("{\"file\":\"~s\",\"comment\":\"~ts\"}", [File, Comment]) || #{file := File, comment := Comment} <- lua()],
    io:format("[~n~ts~n]~n", [string:join(List, ",\n")]);
main(Keys) ->
    io:setopts([{encoding, unicode}]),
    io:setopts(standard_error, [{encoding, unicode}]),
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    Lua = [X || X <- lua(), lists:member(filename:basename(maps:get(file, X), ".lua"), Keys)],
    try
        io:format("~tp~n", [lua_maker:start(Lua)])
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?HALT(Class, Reason, Stacktrace)
    end.

%%%===================================================================
%%% lua data
%%%===================================================================
lua() ->
    [
        #{
            file => "script/make/lua/data/testData.lua",
            comment => "测试配置",
            meta => [
                %% key -> value
                #{name => "zhCN", sql => "SELECT `zhCN` FROM `text_data` WHERE `key` = Key"},
                %% key -> column value
                #{name => "text", sql => "SELECT {*} FROM `text_data` WHERE `key` = Key"},
                %% key -> [value]
                #{name => "type", sql => "SELECT ALL `monster_id` FROM `monster_data` WHERE `type` = Type"},
                %% -> [value] (not unique)
                #{name => "level", sql => "SELECT ALL `level` FROM `level_data` ORDER BY `level` ASC"},
                %% -> [value] (unique)
                #{name => "type_list", sql => "SELECT ALL `type` FROM `monster_data` GROUP BY `type`"},
                %% -> value
                #{name => "min_max_level", sql => "SELECT {MIN(`level`), MAX(`level`)} FROM `level_data`"},
                %% -> value
                #{name => "text_count", sql => "SELECT COUNT(`zhCN`) FROM `text_data`"},
                %% -> value
                #{name => "max_text", sql => "SELECT {MAX(`key`), MAX(`zhCN`)} FROM `text_data`"},
                %% key, key, ... -> value
                #{name => "ref", sql => "SELECT `description` FROM `reference_data` WHERE `key` = Key AND `value` = Value"},
                %% key, key, ... -> value in if else range
                #{name => "ref_range", sql => "SELECT `description` FROM `reference_data` WHERE `key` = Key AND `value` < Value"},
                %% key -> value in if else range ...
                #{name => "get_level_by_exp_asc", sql => "SELECT `level` FROM `level_data` WHERE Exp < `exp` ORDER BY `exp` ASC"},
                % filter data
                #{name => "get", sql => "SELECT `value` FROM `parameter_data` WHERE `key` = Key HAVING `key` LIKE '%size' "}
            ]
        },
        #{
            file => "script/make/lua/data/parameterData.lua",
            comment => "自定义参数配置",
            meta => [
                #{name => "get", sql => "SELECT `value` FROM `parameter_data` WHERE `key` = Key"}
            ]
        }
    ].
