%%%-------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_skill).
-export([main/1]).
-include("../../../include/serialize.hrl").
-include("../../../include/skill.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
main([]) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    io:format("~p~n", [catch protocol_maker:start(protocol())]);
main(_) ->
    io:format("invalid argument~n").

%%%===================================================================
%%% protocol config
%%%===================================================================
protocol() ->
    [#protocol{
        number = 117,
        handler = "src/module/skill/skill_handler.erl",
        erl = "src/module/skill/skill_protocol.erl",
        js = "script/make/protocol/js/SkillProtocol.js",
        lua = "script/make/protocol/lua/SkillProtocol.lua",
        includes = ["skill.hrl"],
        io = [
            #io{
                protocol = 11701,
                comment = "技能列表",
                handler = #handler{module = skill, function = query},
                read = [],
                write = [
                    #list{name = list, comment = "技能列表", explain = #skill{
                        skill_id = #u32{comment = "技能ID"},
                        level = #u16{comment = "技能等级"}
                    }}
                ]
            },
            #io{
                protocol = 11702,
                comment = "学习技能",
                handler = #handler{module = skill, function = learn},
                text = [{configure_not_found, "配置错误"}, {item_not_enough, "材料不足"}, {condition_not_met, "条件不足"}],
                read = [
                    #u32{name = skill_id, comment = "技能ID"}
                ],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            }
        ]
    }].
