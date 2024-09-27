%%%-------------------------------------------------------------------
%%! +pc unicode
%%% @doc
%%% protocol read write define
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_skill).
-export([main/1]).
-include("../../../include/journal.hrl").
-include("../../../include/serialize.hrl").
-include("../../../include/skill.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
main(_) ->
    io:setopts([{encoding, unicode}]),
    io:setopts(standard_error, [{encoding, unicode}]),
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    try
        io:format("~tp~n", [protocol_maker:start(protocol())])
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?HALT(Class, Reason, Stacktrace)
    end.

%%%===================================================================
%%% protocol config
%%%===================================================================
protocol() ->
    #protocol{
        number = 117,
        comment = "技能",
        erl = "script/make/protocol/erl/skill_protocol.erl",
        html = "script/make/protocol/html/SkillProtocol.html",
        lua = "script/make/protocol/lua/SkillProtocol.lua",
        js = "script/make/protocol/js/SkillProtocol.js",
        cs = "script/make/protocol/cs/SkillProtocol.cs",
        io = [
            #io{
                number = 11701,
                comment = "技能列表",
                handler = #handler{module = skill, function = query},
                decode = [],
                encode = [
                    #list{name = list, comment = "技能列表", explain = #skill{
                        skill_id = #u32{comment = "技能ID"},
                        level = #u16{comment = "技能等级"}
                    }}
                ]
            },
            #io{
                number = 11702,
                comment = "学习技能",
                handler = #handler{module = skill, function = learn},
                decode = [
                    #u32{name = skill_id, comment = "技能ID"}
                ],
                encode = [
                    #rst{name = result, comment = "结果"}
                ]
            }
        ]
    }.
