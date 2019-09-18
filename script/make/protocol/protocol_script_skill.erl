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
%%% API
%%%===================================================================
main([]) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    console:stacktrace(catch protocol_maker:start([protocol()]));
main(_) ->
    io:format("invalid argument~n").

%%%===================================================================
%%% protocol config
%%%===================================================================
protocol() ->
    #protocol{
        name = 117,
        handler = "src/module/skill/skill_handler.erl",
        erl = "src/module/skill/skill_protocol.erl",
        json = "script/make/protocol/json/SkillProtocol.js",
        lua = "script/make/protocol/lua/SkillProtocol.lua",
        includes = ["skill.hrl"],
        io = [
            #io{
                name = 11701,
                comment = "技能列表",
                handler = #handler{module = skill, function = query},
                read = [],
                write = [
                    #list{name = list, comment = "技能列表", explain = #skill{
                        skill_id = #u32{comment = "技能ID"},
                        level = #u16{comment = "技能等级"}
                    }}
                ]
            }
        ]
    }.
