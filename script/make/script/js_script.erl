%%%-------------------------------------------------------------------
%%! +pc unicode
%%% @doc
%%% js script for js maker
%%% @end
%%%-------------------------------------------------------------------
-module(js_script).
-export([main/1]).
-include("../../../include/journal.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
main([]) ->
    io:format("[~n~ts~n]~n", [string:join([io_lib:format("{\"file\":\"~s\",\"description\":\"~ts\"}", [filename:basename(element(1, F)), binary_to_list(unicode:characters_to_binary(element(2, F)))]) || F <- js()], ",\n")]);
main(Keys) ->
    io:setopts([{encoding, unicode}]),
    io:setopts(standard_error, [{encoding, unicode}]),
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    Js = [X || X <- js(), lists:member(filename:basename(element(1, X), ".js"), Keys) orelse lists:member(filename:basename(string:replace(element(1, X), "_data", "", trailing), ".js"), Keys)],
    try
        io:format("~tp~n", [js_maker:start(Js)])
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?HALT(Class, Reason, Stacktrace)
    end.

%%%===================================================================
%%% js data
%%%===================================================================
js() ->
    [
        {"script/make/data/js/test_data.js", "测试配置",
            [
                %% key -> value
                {"SELECT `zhCN` FROM `text_data` WHERE `key` = Key", "zhCN"},
                %% key -> column value
                {"SELECT {*} FROM `text_data` WHERE `key` = Key", "text"},
                %% key -> [value]
                {"SELECT ALL `monster_id` FROM `monster_data` WHERE `type` = Type", "type"},
                %% -> [value] (not unique)
                {"SELECT ALL `level` FROM `level_data` ORDER BY `level` ASC", "level"},
                %% -> [value] (unique)
                {"SELECT ALL `type` FROM `monster_data` GROUP BY `type`", "type_list"},
                %% -> value
                {"SELECT {MIN(`level`), MAX(`level`)} FROM `level_data`", "min_max_level"},
                %% -> value
                {"SELECT COUNT(`zhCN`) FROM `text_data`", "text_count"},
                %% -> value
                {"SELECT {MAX(`key`), MAX(`zhCN`)} FROM `text_data`", "max_text"},
                %% key, key, ... -> value
                {"SELECT `description` FROM `reference_data` WHERE `key` = Key AND `value` = Value", "ref"},
                %% key, key, ... -> value in if else range
                {"SELECT `description` FROM `reference_data` WHERE `key` = Key AND `value` < Value", "ref_range"},
                %% key -> value in if else range ...
                {"SELECT `level` FROM `level_data` WHERE Exp < `exp` ORDER BY `exp` ASC", "get_level_by_exp_asc"},
                %% filter data
                {"SELECT `value` FROM `parameter_data` WHERE `key` = Key HAVING `key` LIKE '%size' ", "get"}
            ]
        },
        {"script/make/data/js/parameter_data.js", "自定义参数配置",
            [
                {"SELECT `value` FROM `parameter_data` WHERE `key` = Key", "get"}
            ]
        }
    ].
