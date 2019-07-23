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
    console:stacktrace(catch maker:start(fun lua_maker:parse/2, List)),
    ok;
main(_) ->
    io:format("invail argument~n").

%%%===================================================================
%%% base data
%%%===================================================================
lua() ->
    [
        {"src/module/text/error_code_data.lua",
            [
                {"SELECT `text` FROM `error_code_data` WHERE `protocol` = 'Protocol' AND `code` = 'Code'", ""}
            ]
        },
        {"src/accost/accost_data.lua",
            [
                {"SELECT {`num_id`, `type`, `obj_id`, `hour_start`, `hour_end`} FROM `accost_data` where `day_of_week` = 'DayOfWeek' AND `hour_start` = 'HourStart' AND `hour_end` = 'HourEnd'", ""}
            ]
        }
    ].
