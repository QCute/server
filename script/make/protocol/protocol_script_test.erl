%%%-------------------------------------------------------------------
%%! +pc unicode
%%% @doc
%%% protocol read write define
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_test).
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
        handler = "src/module/test/test_handler.erl",
        erl = "src/module/test/test_protocol.erl",
        html = "script/make/protocol/html/TestProtocol.html",
        lua = "script/make/protocol/lua/TestProtocol.lua",
        js = "script/make/protocol/js/TestProtocol.js",
        cs = "script/make/protocol/cs/TestProtocol.cs",
        includes = [],
        io = [

            %% protocol test
            #io{
                protocol = 65535,
                handler = #handler{module = test, function = test_protocol, protocol = true},
                comment = "协议测试",
                read = [
                    #binary{name = binary, comment = "binary", explain = 6},
                    #bool{name = boolean, comment = "boolean"},

                    #u8{name = u8, comment = "u8"},
                    #u16{name = u16, comment = "u16"},
                    #u32{name = u32, comment = "u32"},
                    #u64{name = u64, comment = "u64"},

                    #i8{name = i8, comment = "i8"},
                    #i16{name = i16, comment = "i16"},
                    #i32{name = i32, comment = "i32"},
                    #i64{name = i64, comment = "i64"},

                    #f32{name = f32, comment = "f32"},
                    #f64{name = f64, comment = "f64"},

                    #str{name = str, comment = "str"},
                    #bst{name = bst, comment = "bst"},

                    #list{name = index_list, comment = "list", explain = {
                        #binary{name = list_binary, comment = "list_binary", explain = 6},
                        #bool{name = list_boolean, comment = "list_boolean"},

                        #u8{name = list_u8, comment = "list_u8"},
                        #u16{name = list_u16, comment = "list_u16"},
                        #u32{name = list_u32, comment = "list_u32"},
                        #u64{name = list_u64, comment = "list_u64"},

                        #i8{name = list_i8, comment = "list_i8"},
                        #i16{name = list_i16, comment = "list_i16"},
                        #i32{name = list_i32, comment = "list_i32"},
                        #i64{name = list_i64, comment = "list_i64"},

                        #f32{name = list_f32, comment = "list_f32"},
                        #f64{name = list_f64, comment = "list_f64"},

                        #str{name = list_str, comment = "list_str"},
                        #bst{name = list_bst, comment = "list_bst"}

                    }},

                    #list{name = key_list, comment = "key_list", key = list_u8, explain = {
                        #binary{name = list_binary, comment = "list_binary", explain = 6},
                        #bool{name = list_boolean, comment = "list_boolean"},

                        #u8{name = list_u8, comment = "list_u8"},
                        #u16{name = list_u16, comment = "list_u16"},
                        #u32{name = list_u32, comment = "list_u32"},
                        #u64{name = list_u64, comment = "list_u64"},

                        #i8{name = list_i8, comment = "list_i8"},
                        #i16{name = list_i16, comment = "list_i16"},
                        #i32{name = list_i32, comment = "list_i32"},
                        #i64{name = list_i64, comment = "list_i64"},

                        #f32{name = list_f32, comment = "list_f32"},
                        #f64{name = list_f64, comment = "list_f64"},

                        #str{name = list_str, comment = "list_str"},
                        #bst{name = list_bst, comment = "list_bst"}

                    }}

                ],
                write = [
                    #binary{name = binary, comment = "binary", explain = 6},
                    #bool{name = boolean, comment = "boolean"},

                    #u8{name = u8, comment = "u8"},
                    #u16{name = u16, comment = "u16"},
                    #u32{name = u32, comment = "u32"},
                    #u64{name = u64, comment = "u64"},

                    #i8{name = i8, comment = "i8"},
                    #i16{name = i16, comment = "i16"},
                    #i32{name = i32, comment = "i32"},
                    #i64{name = i64, comment = "i64"},

                    #f32{name = f32, comment = "f32"},
                    #f64{name = f64, comment = "f64"},

                    #str{name = str, comment = "str"},
                    #bst{name = bst, comment = "bst"},

                    #list{name = index_list, comment = "list", explain = {
                        #binary{name = list_binary, comment = "list_binary", explain = 6},
                        #bool{name = list_boolean, comment = "list_boolean"},

                        #u8{name = list_u8, comment = "list_u8"},
                        #u16{name = list_u16, comment = "list_u16"},
                        #u32{name = list_u32, comment = "list_u32"},
                        #u64{name = list_u64, comment = "list_u64"},

                        #i8{name = list_i8, comment = "list_i8"},
                        #i16{name = list_i16, comment = "list_i16"},
                        #i32{name = list_i32, comment = "list_i32"},
                        #i64{name = list_i64, comment = "list_i64"},

                        #f32{name = list_f32, comment = "list_f32"},
                        #f64{name = list_f64, comment = "list_f64"},

                        #str{name = list_str, comment = "list_str"},
                        #bst{name = list_bst, comment = "list_bst"}

                    }},

                    #list{name = key_list, comment = "key_list", key = list_u8, explain = {
                        #binary{name = list_binary, comment = "list_binary", explain = 6},
                        #bool{name = list_boolean, comment = "list_boolean"},

                        #u8{name = list_u8, comment = "list_u8"},
                        #u16{name = list_u16, comment = "list_u16"},
                        #u32{name = list_u32, comment = "list_u32"},
                        #u64{name = list_u64, comment = "list_u64"},

                        #i8{name = list_i8, comment = "list_i8"},
                        #i16{name = list_i16, comment = "list_i16"},
                        #i32{name = list_i32, comment = "list_i32"},
                        #i64{name = list_i64, comment = "list_i64"},

                        #f32{name = list_f32, comment = "list_f32"},
                        #f64{name = list_f64, comment = "list_f64"},

                        #str{name = list_str, comment = "list_str"},
                        #bst{name = list_bst, comment = "list_bst"}

                    }}

                ]
            }
        ]
    }.
