export default {
    "65535" : {
        "comment" : "协议测试",
        "write" : [
            {"name": "binary", "type": "binary", "comment": "binary", "explain": 6},
            {"name": "boolean", "type": "bool", "comment": "boolean", "explain": []},
            {"name": "u8", "type": "u8", "comment": "u8", "explain": []},
            {"name": "u16", "type": "u16", "comment": "u16", "explain": []},
            {"name": "u32", "type": "u32", "comment": "u32", "explain": []},
            {"name": "u64", "type": "u64", "comment": "u64", "explain": []},
            {"name": "i8", "type": "i8", "comment": "i8", "explain": []},
            {"name": "i16", "type": "i16", "comment": "i16", "explain": []},
            {"name": "i32", "type": "i32", "comment": "i32", "explain": []},
            {"name": "i64", "type": "i64", "comment": "i64", "explain": []},
            {"name": "f32", "type": "f32", "comment": "f32", "explain": []},
            {"name": "f64", "type": "f64", "comment": "f64", "explain": []},
            {"name": "str", "type": "str", "comment": "str", "explain": []},
            {"name": "bst", "type": "bst", "comment": "bst", "explain": []},
            {"name": "tuple", "type": "tuple", "comment": "tuple", "explain": [
                {"name": "tupleBinary", "type": "binary", "comment": "tuple_binary", "explain": 6},
                {"name": "tupleSubTuple", "type": "tuple", "comment": "tuple_sub_tuple", "explain": [
                    {"name": "tupleSubTupleU8", "type": "u8", "comment": "tuple_sub_tuple_u8", "explain": []},
                    {"name": "tupleSubTupleStr", "type": "str", "comment": "tuple_sub_tuple_str", "explain": []}
                ]},
                {"name": "tupleSubList", "type": "list", "comment": "tuple_sub_list", "explain": [
                    {"name": "", "type": "tuple", "comment": "", "explain": [
                        {"name": "tupleSubListI16", "type": "i16", "comment": "tuple_sub_list_i16", "explain": []},
                        {"name": "tupleSubListBst", "type": "bst", "comment": "tuple_sub_list_bst", "explain": []}
                    ]}
                ]},
                {"name": "tupleSubListSingle", "type": "list", "comment": "tuple_sub_list_single", "explain": 
                    {"name": "tupleSubListSingleBool", "type": "bool", "comment": "tuple_sub_list_single_bool", "explain": []}
                }
            ]},
            {"name": "indexList", "type": "list", "comment": "list", "explain": [
                {"name": "", "type": "tuple", "comment": "", "explain": [
                    {"name": "listBinary", "type": "binary", "comment": "list_binary", "explain": 6},
                    {"name": "listSubTuple", "type": "tuple", "comment": "list_sub_tuple", "explain": [
                        {"name": "listSubTupleU8", "type": "u8", "comment": "list_sub_tuple_u8", "explain": []},
                        {"name": "listSubTupleStr", "type": "str", "comment": "list_sub_tuple_str", "explain": []}
                    ]},
                    {"name": "listSubList", "type": "list", "comment": "list_sub_list", "explain": [
                        {"name": "", "type": "tuple", "comment": "", "explain": [
                            {"name": "listSubListI16", "type": "i16", "comment": "list_sub_list_i16", "explain": []},
                            {"name": "listSubListBst", "type": "bst", "comment": "list_sub_list_bst", "explain": []}
                        ]}
                    ]},
                    {"name": "listSubListSingle", "type": "list", "comment": "list_sub_list_single", "explain": 
                        {"name": "listSubListSingleBool", "type": "bool", "comment": "list_sub_list_single_bool", "explain": []}
                    }
                ]}
            ]},
            {"name": "keyList", "type": "map", "comment": "key_list", "key": "listU8", "explain": [
                {"name": "", "type": "tuple", "comment": "", "explain": [
                    {"name": "listBinary", "type": "binary", "comment": "list_binary", "explain": 6},
                    {"name": "listBoolean", "type": "bool", "comment": "list_boolean", "explain": []},
                    {"name": "listU8", "type": "u8", "comment": "list_u8", "explain": []},
                    {"name": "listU16", "type": "u16", "comment": "list_u16", "explain": []},
                    {"name": "listU32", "type": "u32", "comment": "list_u32", "explain": []},
                    {"name": "listU64", "type": "u64", "comment": "list_u64", "explain": []},
                    {"name": "listI8", "type": "i8", "comment": "list_i8", "explain": []},
                    {"name": "listI16", "type": "i16", "comment": "list_i16", "explain": []},
                    {"name": "listI32", "type": "i32", "comment": "list_i32", "explain": []},
                    {"name": "listI64", "type": "i64", "comment": "list_i64", "explain": []},
                    {"name": "listF32", "type": "f32", "comment": "list_f32", "explain": []},
                    {"name": "listF64", "type": "f64", "comment": "list_f64", "explain": []},
                    {"name": "listStr", "type": "str", "comment": "list_str", "explain": []},
                    {"name": "listBst", "type": "bst", "comment": "list_bst", "explain": []}
                ]}
            ]}
        ],
        "read" : [
            {"name": "binary", "type": "binary", "comment": "binary", "explain": 6},
            {"name": "boolean", "type": "bool", "comment": "boolean", "explain": []},
            {"name": "u8", "type": "u8", "comment": "u8", "explain": []},
            {"name": "u16", "type": "u16", "comment": "u16", "explain": []},
            {"name": "u32", "type": "u32", "comment": "u32", "explain": []},
            {"name": "u64", "type": "u64", "comment": "u64", "explain": []},
            {"name": "i8", "type": "i8", "comment": "i8", "explain": []},
            {"name": "i16", "type": "i16", "comment": "i16", "explain": []},
            {"name": "i32", "type": "i32", "comment": "i32", "explain": []},
            {"name": "i64", "type": "i64", "comment": "i64", "explain": []},
            {"name": "f32", "type": "f32", "comment": "f32", "explain": []},
            {"name": "f64", "type": "f64", "comment": "f64", "explain": []},
            {"name": "str", "type": "str", "comment": "str", "explain": []},
            {"name": "bst", "type": "bst", "comment": "bst", "explain": []},
            {"name": "tuple", "type": "tuple", "comment": "tuple", "explain": [
                {"name": "tupleBinary", "type": "binary", "comment": "tuple_binary", "explain": 6},
                {"name": "tupleSubTuple", "type": "tuple", "comment": "tuple_sub_tuple", "explain": [
                    {"name": "tupleSubTupleU8", "type": "u8", "comment": "tuple_sub_tuple_u8", "explain": []},
                    {"name": "tupleSubTupleStr", "type": "str", "comment": "tuple_sub_tuple_str", "explain": []}
                ]},
                {"name": "tupleSubList", "type": "list", "comment": "tuple_sub_list", "explain": [
                    {"name": "", "type": "tuple", "comment": "", "explain": [
                        {"name": "tupleSubListI16", "type": "i16", "comment": "tuple_sub_list_i16", "explain": []},
                        {"name": "tupleSubListBst", "type": "bst", "comment": "tuple_sub_list_bst", "explain": []}
                    ]}
                ]},
                {"name": "tupleSubListSingle", "type": "list", "comment": "tuple_sub_list_single", "explain": 
                    {"name": "tupleSubListSingleBool", "type": "bool", "comment": "tuple_sub_list_single_bool", "explain": []}
                }
            ]},
            {"name": "indexList", "type": "list", "comment": "list", "explain": [
                {"name": "", "type": "tuple", "comment": "", "explain": [
                    {"name": "listBinary", "type": "binary", "comment": "list_binary", "explain": 6},
                    {"name": "listSubTuple", "type": "tuple", "comment": "list_sub_tuple", "explain": [
                        {"name": "listSubTupleU8", "type": "u8", "comment": "list_sub_tuple_u8", "explain": []},
                        {"name": "listSubTupleStr", "type": "str", "comment": "list_sub_tuple_str", "explain": []}
                    ]},
                    {"name": "listSubList", "type": "list", "comment": "list_sub_list", "explain": [
                        {"name": "", "type": "tuple", "comment": "", "explain": [
                            {"name": "listSubListI16", "type": "i16", "comment": "list_sub_list_i16", "explain": []},
                            {"name": "listSubListBst", "type": "bst", "comment": "list_sub_list_bst", "explain": []}
                        ]}
                    ]},
                    {"name": "listSubListSingle", "type": "list", "comment": "list_sub_list_single", "explain": 
                        {"name": "listSubListSingleBool", "type": "bool", "comment": "list_sub_list_single_bool", "explain": []}
                    }
                ]}
            ]},
            {"name": "keyList", "type": "map", "comment": "key_list", "key": "listU8", "explain": [
                {"name": "", "type": "tuple", "comment": "", "explain": [
                    {"name": "listBinary", "type": "binary", "comment": "list_binary", "explain": 6},
                    {"name": "listBoolean", "type": "bool", "comment": "list_boolean", "explain": []},
                    {"name": "listU8", "type": "u8", "comment": "list_u8", "explain": []},
                    {"name": "listU16", "type": "u16", "comment": "list_u16", "explain": []},
                    {"name": "listU32", "type": "u32", "comment": "list_u32", "explain": []},
                    {"name": "listU64", "type": "u64", "comment": "list_u64", "explain": []},
                    {"name": "listI8", "type": "i8", "comment": "list_i8", "explain": []},
                    {"name": "listI16", "type": "i16", "comment": "list_i16", "explain": []},
                    {"name": "listI32", "type": "i32", "comment": "list_i32", "explain": []},
                    {"name": "listI64", "type": "i64", "comment": "list_i64", "explain": []},
                    {"name": "listF32", "type": "f32", "comment": "list_f32", "explain": []},
                    {"name": "listF64", "type": "f64", "comment": "list_f64", "explain": []},
                    {"name": "listStr", "type": "str", "comment": "list_str", "explain": []},
                    {"name": "listBst", "type": "bst", "comment": "list_bst", "explain": []}
                ]}
            ]}
        ]
    }
};