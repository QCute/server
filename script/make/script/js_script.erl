%%%-------------------------------------------------------------------
%%! +pc unicode
%%% @doc
%%% js script for js maker
%%% @end
%%%-------------------------------------------------------------------
-module(js_script).
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
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    List = [X || X <- js(), filename:basename(element(1, X), ".js") == Key orelse filename:basename(element(1, X), ".js") == Key ++ "_data"],
    try
        io:format("~p~n", [js_maker:start(List)])
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?ERROR_STACKTRACE(Class, Reason, Stacktrace)
    end;
main([]) ->
    io:format("[~n~ts~n]~n", [string:join([io_lib:format("{\"file\":\"~s\",\"description\":\"~ts\"}", [filename:basename(element(1, F)), binary_to_list(unicode:characters_to_binary(element(2, F)))]) || F <- js()], ",\n")]);
main(Args) ->
    io:format(standard_error, "invalid argument: ~p~n", [Args]).

%%%===================================================================
%%% js data
%%%===================================================================
js() ->
    [
        {"script/make/data/js/test_data.js", "测试配置",
            [
                {"SELECT `en` FROM `text_data` WHERE `key` = Key", "sc"},
                {"SELECT {*} FROM `text_data` WHERE `key` = Key", "text"},
                {"SELECT ALL `level` FROM `level_data` ORDER BY `level` ASC", "level"},
                {"SELECT `tc` FROM `error_code_data` WHERE `key` = Key AND `type` = Type ", "tc"},
                {"SELECT ALL `monster_id` FROM `monster_data` WHERE `type` = Type ", "type"},
                {"SELECT ALL `type` FROM `monster_data` GROUP BY `type` ", "type_list"},
                {"SELECT MAX(`level`) FROM `level_data` ", "max_level"},
                {"SELECT COUNT(`exp`) FROM `level_data` ", "level_count"},
                {"SELECT COUNT(`en`) FROM `text_data` ", "text_count"},
                {"SELECT `level` FROM `level_data` WHERE Exp = `exp` ORDER BY `exp` DESC DEFAULT 0 ", "get_level_by_exp"},
                {"SELECT #record{*} FROM `quest_data` WHERE `quest_id` = QuestId", "get"}
            ]
        },
        {"script/make/data/js/parameter_data.js", "自定义参数配置",
            [
                {"SELECT `value` FROM `parameter_data` WHERE `key` = Key", "get"}
            ]
        }
    ].
