%%%-------------------------------------------------------------------
%%% @doc
%%% module data script
%%% @end
%%%-------------------------------------------------------------------
-module(data_script).
-export([main/1]).
%% ------------------------ user guide -------------------------------
%%
%% sql      :: auto group by key(when key reduplicated)
%% type     :: list | maps | tuple |    record     | origin(default)
%% type     :: []   | #{}  |   {}  | ()/#record{}  |
%% default  :: [] | record | maps | tuple | list | (specified value)
%% includes :: ["*.hrl", "*.hrl"]
%%%===================================================================
%%% API
%%%===================================================================
main([Key]) ->
    code:add_path("beam"),
    code:add_path("../beam"),
    code:add_path("../../beam"),
    code:add_path("../../../beam"),
    List = [X || X <- data(), string:str(element(1, X), Key) =/= 0],
    console:stack_trace(catch maker:start(fun data_maker:parse/2, List)),
    ok;
main(_) ->
    io:format("invail argument~n").

%%%===================================================================
%%% base data
%%%===================================================================
data() ->
    [
        {"src/data/data_param.erl", [], 
            [
                {"select `value` from `data_parameter` where `key` = 'Key'", "get", []}
            ]
        },
        {"src/data/data_vip.erl", ["vip.hrl"],
            [
                {"SELECT `vip` FROM `data_vip` where 'Money' < `money` order by `money` asc;", "get", 0},
                {"SELECT `vip` FROM `data_vip` group by `vip` order by `money` asc;", "list", []}
            ]
        },
        {"src/data/data_player.erl", ["player.hrl"],
            [
                {"SELECT `level` FROM `data_level` where 'Exp' < `exp` order by `exp` asc;", "level", 0}
            ]
        },
        {"src/data/data_guild.erl", ["guild.hrl"],
            [
                {"SELECT `value` FROM `data_guild_param` where `type` = 'Type' AND `param` = 'Param'", "param", []}
            ]
        }
    ].