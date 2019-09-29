%%%-------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_secret).
-export([main/1]).
-include("../../../include/serialize.hrl").
%%%===================================================================
%%% API 
%%%===================================================================
main([]) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    console:stacktrace( protocol_maker:start([protocol()]));
main(_) ->
    io:format("invalid argument~n").

%%%===================================================================
%%% protocol config
%%%===================================================================
protocol() ->
    #protocol{
        name = 600,
        handler = "src/module/secret/secret_handler.erl",
        erl = "src/module/secret/secret_protocol.erl",
        lua = "script/make/protocol/lua/SecretProtocol.lua",
        json = "script/make/protocol/json/SecretProtocol.js",
        includes = [],
        io = [
            #io{
                name = 60000,
                comment = "秘籍",
                handler = #handler{module = secret, function = cheat},
                read = [
                    #str{name = command, comment = "命令"}
                ],
                write = [
                    #u8{name = result, comment = "结果(0:失败/1:成功)"},
                    #str{name = command, comment = "命令"}
                ]
            }
        ]
    }.
