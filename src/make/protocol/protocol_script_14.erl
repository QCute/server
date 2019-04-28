%%%-------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_14).
-export([main/1]).
-include("../../../include/serialize.hrl").
-include("../../../include/mail.hrl").
%%%===================================================================
%%% API
%%%===================================================================
main([]) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    Protocol = #protocol{file = File} = protocol(),
    console:stacktrace(catch maker:start(fun protocol_maker:parse/2, [{File, Protocol}])),
    ok;
main(_) ->
    io:format("invail argument~n").

%%%===================================================================
%%% protocol config
%%%===================================================================
protocol() ->
    #protocol{
        file = "src/protocol/protocol_14.erl",
        include = ["mail.hrl"],
        io = [
            #io{
                name = 14001,
                comment = "Mail",
                read = [],
                write = [
                    #list{name = list, desc = #mail{                         %% 邮件列表
                        id = #u64{},                                         %% |-- ID
                        sender_id = #u64{},                                  %% |-- 发送者
                        sender_nick = #bin{},                                %% |-- 发送者昵称
                        receiver_id = #u64{},                                %% |-- 接收者(select)
                        receiver_nick = #bin{},                              %% |-- 接受者昵称
                        is_read = #u8{},                                     %% |-- 是否已经读取(update_read)
                        read_time = #u32{},                                  %% |-- 读取时间(update_read)
                        receive_time = #u32{},                               %% |-- 接收时间
                        valid_time = #u32{},                                 %% |-- 有效时间
                        title = #bin{},                                      %% |-- 标题
                        content = #bin{},                                    %% |-- 内容
                        attachment = #list{name = item, desc = {             %% |-- 附件列表
                            #u32{name = item_id},                            %% |-- |-- 物品配置ID
                            #u16{name = amount},                             %% |-- |-- 数量
                            #u8{name = bind}                                 %% |-- |-- 绑定
                        }}
                    }}
                ]
            }
        ]
    }.
