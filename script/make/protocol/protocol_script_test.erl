%%%-------------------------------------------------------------------
%%! +pc unicode -pa beam
%%% @doc
%%% protocol read write define
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_test).
-mode(compile).
-compile({parse_transform, protocol_maker_transform}).
-export([main/1]).
-include("../../../include/journal.hrl").
-include("../../../include/serialize.hrl").
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
        number = 655,
        comment = "测试",
        erl = "script/make/protocol/erl/test_protocol.erl",
        html = "script/make/protocol/html/TestProtocol.html",
        lua = "script/make/protocol/lua/TestProtocol.lua",
        js = "script/make/protocol/js/TestProtocol.js",
        cs = "script/make/protocol/cs/TestProtocol.cs",
        io = [
            %% protocol test
            #io{
                number = 65535,
                handler = #handler{module = test, function = test_protocol, protocol = true},
                comment = "协议测试",
                decode = {
                    binary = binary(6),                    %% binary
                    bool = bool(),                         %% bool

                    u8 = u8(),                             %% u8
                    u16 = u16(),                           %% u16
                    u32 = u32(),                           %% u32
                    u64 = u64(),                           %% u64
                    i8 = i8(),                             %% i8
                    i16 = i16(),                           %% i16
                    i32 = i32(),                           %% i32
                    i64 = i64(),                           %% i16
                    f32 = f32(),                           %% f32
                    f64 = f64(),                           %% f64
                    str = str(),                           %% str
                    bst = bst(),                           %% bst

                    %% meta(bst, bst(), "bst")
                    %% meta(tuple, {...}, "tuple")
                    tuple = {                              %% tuple
                        binary = binary(6),                %% tuple binary

                        sub = {                            %% tuple tuple
                            u8 = u8(),                     %% tuple tuple u8
                            str = str()                    %% tuple tuple str
                        },

                        list = [                           %% tuple list
                            {
                                i16 = i16(),               %% tuple list i16
                                bst = bst()                %% tuple list bst
                            }
                        ],

                        single = [
                            bool()                         %% bool
                        ]
                    },

                    %% meta(index_list, [object("", #{...}, "")], "list")
                    index_list = [                         %% list
                        {
                            binary = binary(6),            %% tuple binary

                            sub = {                        %% tuple tuple
                                u8 = u8(),                 %% tuple tuple u8
                                str = str()                %% tuple tuple str
                            },

                            list = [                       %% tuple list
                                {
                                    i16 = i16(),           %% tuple list i16
                                    bst = bst()            %% tuple list bst
                                }
                            ],

                            single = [
                                bool()                     %% bool
                            ]
                        }
                    ],

                    %% meta(key_list, [map(u8, #{...}, "")], "list")
                    key_list = [
                        u8 = {
                            binary = binary(6),            %% binary
                            bool = bool(),                 %% bool

                            u8 = u8(),                     %% u8
                            u16 = u16(),                   %% u16
                            u32 = u32(),                   %% u32
                            u64 = u64(),                   %% u64
                            i8 = i8(),                     %% i8
                            i16 = i16(),                   %% i16
                            i32 = i32(),                   %% i32
                            i64 = i64(),                   %% i64
                            f32 = f32(),                   %% f32
                            f64 = f64(),                   %% f64
                            str = str(),                   %% str
                            bst = bst()                    %% bst
                        }
                    ]
                },
                encode = {
                    binary = binary(6),                    %% binary
                    bool = bool(),                         %% bool

                    u8 = u8(),                             %% u8
                    u16 = u16(),                           %% u16
                    u32 = u32(),                           %% u32
                    u64 = u64(),                           %% u64
                    i8 = i8(),                             %% i8
                    i16 = i16(),                           %% i16
                    i32 = i32(),                           %% i32
                    i64 = i64(),                           %% i16
                    f32 = f32(),                           %% f32
                    f64 = f64(),                           %% f64
                    str = str(),                           %% str
                    bst = bst(),                           %% bst

                    %% meta(bst, bst(), "bst")
                    %% meta(tuple, {...}, "tuple")
                    tuple = {                              %% tuple
                        binary = binary(6),                %% tuple binary

                        sub = {                            %% tuple tuple
                            u8 = u8(),                     %% tuple tuple u8
                            str = str()                    %% tuple tuple str
                        },

                        list = [                           %% tuple list
                            {
                                i16 = i16(),               %% tuple list i16
                                bst = bst()                %% tuple list bst
                            }
                        ],

                        single = [
                            bool()                         %% bool
                        ]
                    },

                    %% meta(index_list, [object("", #{...}, "")], "list")
                    index_list = [                         %% list
                        {
                            binary = binary(6),            %% tuple binary

                            sub = {                        %% tuple tuple
                                u8 = u8(),                 %% tuple tuple u8
                                str = str()                %% tuple tuple str
                            },

                            list = [                       %% tuple list
                                {
                                    i16 = i16(),           %% tuple list i16
                                    bst = bst()            %% tuple list bst
                                }
                            ],

                            single = [
                                bool()                     %% bool
                            ]
                        }
                    ],

                    %% meta(key_list, [map(u8, #{...}, "")], "list")
                    key_list = [
                        u8 = {
                            binary = binary(6),            %% binary
                            bool = bool(),                 %% bool

                            u8 = u8(),                     %% u8
                            u16 = u16(),                   %% u16
                            u32 = u32(),                   %% u32
                            u64 = u64(),                   %% u64
                            i8 = i8(),                     %% i8
                            i16 = i16(),                   %% i16
                            i32 = i32(),                   %% i32
                            i64 = i64(),                   %% i64
                            f32 = f32(),                   %% f32
                            f64 = f64(),                   %% f64
                            str = str(),                   %% str
                            bst = bst()                    %% bst
                        }
                    ]
                }
            }
        ]
    }.
