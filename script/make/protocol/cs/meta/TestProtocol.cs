using List = System.Collections.Generic.List<System.Object>;
using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;

public static class TestProtocol
{
    public static Map GetMeta()
    {
        return new Map()
        {
            {"65535", new Map() {
                {"comment", "协议测试"},
                {"write", new List() {
                    new Map() { {"name", "binary"}, {"type", "binary"}, {"comment", "binary"}, {"explain", 6} },
                    new Map() { {"name", "boolean"}, {"type", "bool"}, {"comment", "boolean"}, {"explain", new List()} },
                    new Map() { {"name", "u8"}, {"type", "u8"}, {"comment", "u8"}, {"explain", new List()} },
                    new Map() { {"name", "u16"}, {"type", "u16"}, {"comment", "u16"}, {"explain", new List()} },
                    new Map() { {"name", "u32"}, {"type", "u32"}, {"comment", "u32"}, {"explain", new List()} },
                    new Map() { {"name", "u64"}, {"type", "u64"}, {"comment", "u64"}, {"explain", new List()} },
                    new Map() { {"name", "i8"}, {"type", "i8"}, {"comment", "i8"}, {"explain", new List()} },
                    new Map() { {"name", "i16"}, {"type", "i16"}, {"comment", "i16"}, {"explain", new List()} },
                    new Map() { {"name", "i32"}, {"type", "i32"}, {"comment", "i32"}, {"explain", new List()} },
                    new Map() { {"name", "i64"}, {"type", "i64"}, {"comment", "i64"}, {"explain", new List()} },
                    new Map() { {"name", "f32"}, {"type", "f32"}, {"comment", "f32"}, {"explain", new List()} },
                    new Map() { {"name", "f64"}, {"type", "f64"}, {"comment", "f64"}, {"explain", new List()} },
                    new Map() { {"name", "str"}, {"type", "str"}, {"comment", "str"}, {"explain", new List()} },
                    new Map() { {"name", "bst"}, {"type", "bst"}, {"comment", "bst"}, {"explain", new List()} },
                    new Map() { {"name", "tuple"}, {"type", "tuple"}, {"comment": "tuple"}, {"explain": new List() {
                        new Map() { {"name", "tupleBinary"}, {"type", "binary"}, {"comment", "tuple_binary"}, {"explain", 6} },
                        new Map() { {"name", "tupleSubTuple"}, {"type", "tuple"}, {"comment": "tuple_sub_tuple"}, {"explain": new List() {
                            new Map() { {"name", "tupleSubTupleU8"}, {"type", "u8"}, {"comment", "tuple_sub_tuple_u8"}, {"explain", new List()} },
                            new Map() { {"name", "tupleSubTupleStr"}, {"type", "str"}, {"comment", "tuple_sub_tuple_str"}, {"explain", new List()} }
                        }}},
                        new Map() { {"name", "tupleSubList"}, {"type", "list"}, {"comment", "tuple_sub_list"}, {"explain", new List() {
                            new Map() { {"name", ""}, {"type", "tuple"}, {"comment": ""}, {"explain": new List() {
                                new Map() { {"name", "tupleSubListI16"}, {"type", "i16"}, {"comment", "tuple_sub_list_i16"}, {"explain", new List()} },
                                new Map() { {"name", "tupleSubListBst"}, {"type", "bst"}, {"comment", "tuple_sub_list_bst"}, {"explain", new List()} }
                            }}}
                        }}},
                        new Map() { {"name", "tupleSubListSingle"}, {"type", "list"}, {"comment", "tuple_sub_list_single"}, {"explain", 
                            new Map() { {"name", "tupleSubListSingleBool"}, {"type", "bool"}, {"comment", "tuple_sub_list_single_bool"}, {"explain", new List()} }
                        }}
                    }}},
                    new Map() { {"name", "indexList"}, {"type", "list"}, {"comment", "list"}, {"explain", new List() {
                        new Map() { {"name", ""}, {"type", "tuple"}, {"comment": ""}, {"explain": new List() {
                            new Map() { {"name", "listBinary"}, {"type", "binary"}, {"comment", "list_binary"}, {"explain", 6} },
                            new Map() { {"name", "listSubTuple"}, {"type", "tuple"}, {"comment": "list_sub_tuple"}, {"explain": new List() {
                                new Map() { {"name", "listSubTupleU8"}, {"type", "u8"}, {"comment", "list_sub_tuple_u8"}, {"explain", new List()} },
                                new Map() { {"name", "listSubTupleStr"}, {"type", "str"}, {"comment", "list_sub_tuple_str"}, {"explain", new List()} }
                            }}},
                            new Map() { {"name", "listSubList"}, {"type", "list"}, {"comment", "list_sub_list"}, {"explain", new List() {
                                new Map() { {"name", ""}, {"type", "tuple"}, {"comment": ""}, {"explain": new List() {
                                    new Map() { {"name", "listSubListI16"}, {"type", "i16"}, {"comment", "list_sub_list_i16"}, {"explain", new List()} },
                                    new Map() { {"name", "listSubListBst"}, {"type", "bst"}, {"comment", "list_sub_list_bst"}, {"explain", new List()} }
                                }}}
                            }}},
                            new Map() { {"name", "listSubListSingle"}, {"type", "list"}, {"comment", "list_sub_list_single"}, {"explain", 
                                new Map() { {"name", "listSubListSingleBool"}, {"type", "bool"}, {"comment", "list_sub_list_single_bool"}, {"explain", new List()} }
                            }}
                        }}}
                    }}},
                    new Map() { {"name", "keyList"}, {"type", "map"}, {"comment", "key_list"}, {"key", "listU8"}, {"explain", new List() {
                        new Map() { {"name", ""}, {"type", "tuple"}, {"comment": ""}, {"explain": new List() {
                            new Map() { {"name", "listBinary"}, {"type", "binary"}, {"comment", "list_binary"}, {"explain", 6} },
                            new Map() { {"name", "listBoolean"}, {"type", "bool"}, {"comment", "list_boolean"}, {"explain", new List()} },
                            new Map() { {"name", "listU8"}, {"type", "u8"}, {"comment", "list_u8"}, {"explain", new List()} },
                            new Map() { {"name", "listU16"}, {"type", "u16"}, {"comment", "list_u16"}, {"explain", new List()} },
                            new Map() { {"name", "listU32"}, {"type", "u32"}, {"comment", "list_u32"}, {"explain", new List()} },
                            new Map() { {"name", "listU64"}, {"type", "u64"}, {"comment", "list_u64"}, {"explain", new List()} },
                            new Map() { {"name", "listI8"}, {"type", "i8"}, {"comment", "list_i8"}, {"explain", new List()} },
                            new Map() { {"name", "listI16"}, {"type", "i16"}, {"comment", "list_i16"}, {"explain", new List()} },
                            new Map() { {"name", "listI32"}, {"type", "i32"}, {"comment", "list_i32"}, {"explain", new List()} },
                            new Map() { {"name", "listI64"}, {"type", "i64"}, {"comment", "list_i64"}, {"explain", new List()} },
                            new Map() { {"name", "listF32"}, {"type", "f32"}, {"comment", "list_f32"}, {"explain", new List()} },
                            new Map() { {"name", "listF64"}, {"type", "f64"}, {"comment", "list_f64"}, {"explain", new List()} },
                            new Map() { {"name", "listStr"}, {"type", "str"}, {"comment", "list_str"}, {"explain", new List()} },
                            new Map() { {"name", "listBst"}, {"type", "bst"}, {"comment", "list_bst"}, {"explain", new List()} }
                        }}}
                    }}}
                }},
                {"read", new List() {
                    new Map() { {"name", "binary"}, {"type", "binary"}, {"comment", "binary"}, {"explain", 6} },
                    new Map() { {"name", "boolean"}, {"type", "bool"}, {"comment", "boolean"}, {"explain", new List()} },
                    new Map() { {"name", "u8"}, {"type", "u8"}, {"comment", "u8"}, {"explain", new List()} },
                    new Map() { {"name", "u16"}, {"type", "u16"}, {"comment", "u16"}, {"explain", new List()} },
                    new Map() { {"name", "u32"}, {"type", "u32"}, {"comment", "u32"}, {"explain", new List()} },
                    new Map() { {"name", "u64"}, {"type", "u64"}, {"comment", "u64"}, {"explain", new List()} },
                    new Map() { {"name", "i8"}, {"type", "i8"}, {"comment", "i8"}, {"explain", new List()} },
                    new Map() { {"name", "i16"}, {"type", "i16"}, {"comment", "i16"}, {"explain", new List()} },
                    new Map() { {"name", "i32"}, {"type", "i32"}, {"comment", "i32"}, {"explain", new List()} },
                    new Map() { {"name", "i64"}, {"type", "i64"}, {"comment", "i64"}, {"explain", new List()} },
                    new Map() { {"name", "f32"}, {"type", "f32"}, {"comment", "f32"}, {"explain", new List()} },
                    new Map() { {"name", "f64"}, {"type", "f64"}, {"comment", "f64"}, {"explain", new List()} },
                    new Map() { {"name", "str"}, {"type", "str"}, {"comment", "str"}, {"explain", new List()} },
                    new Map() { {"name", "bst"}, {"type", "bst"}, {"comment", "bst"}, {"explain", new List()} },
                    new Map() { {"name", "tuple"}, {"type", "tuple"}, {"comment": "tuple"}, {"explain": new List() {
                        new Map() { {"name", "tupleBinary"}, {"type", "binary"}, {"comment", "tuple_binary"}, {"explain", 6} },
                        new Map() { {"name", "tupleSubTuple"}, {"type", "tuple"}, {"comment": "tuple_sub_tuple"}, {"explain": new List() {
                            new Map() { {"name", "tupleSubTupleU8"}, {"type", "u8"}, {"comment", "tuple_sub_tuple_u8"}, {"explain", new List()} },
                            new Map() { {"name", "tupleSubTupleStr"}, {"type", "str"}, {"comment", "tuple_sub_tuple_str"}, {"explain", new List()} }
                        }}},
                        new Map() { {"name", "tupleSubList"}, {"type", "list"}, {"comment", "tuple_sub_list"}, {"explain", new List() {
                            new Map() { {"name", ""}, {"type", "tuple"}, {"comment": ""}, {"explain": new List() {
                                new Map() { {"name", "tupleSubListI16"}, {"type", "i16"}, {"comment", "tuple_sub_list_i16"}, {"explain", new List()} },
                                new Map() { {"name", "tupleSubListBst"}, {"type", "bst"}, {"comment", "tuple_sub_list_bst"}, {"explain", new List()} }
                            }}}
                        }}},
                        new Map() { {"name", "tupleSubListSingle"}, {"type", "list"}, {"comment", "tuple_sub_list_single"}, {"explain", 
                            new Map() { {"name", "tupleSubListSingleBool"}, {"type", "bool"}, {"comment", "tuple_sub_list_single_bool"}, {"explain", new List()} }
                        }}
                    }}},
                    new Map() { {"name", "indexList"}, {"type", "list"}, {"comment", "list"}, {"explain", new List() {
                        new Map() { {"name", ""}, {"type", "tuple"}, {"comment": ""}, {"explain": new List() {
                            new Map() { {"name", "listBinary"}, {"type", "binary"}, {"comment", "list_binary"}, {"explain", 6} },
                            new Map() { {"name", "listSubTuple"}, {"type", "tuple"}, {"comment": "list_sub_tuple"}, {"explain": new List() {
                                new Map() { {"name", "listSubTupleU8"}, {"type", "u8"}, {"comment", "list_sub_tuple_u8"}, {"explain", new List()} },
                                new Map() { {"name", "listSubTupleStr"}, {"type", "str"}, {"comment", "list_sub_tuple_str"}, {"explain", new List()} }
                            }}},
                            new Map() { {"name", "listSubList"}, {"type", "list"}, {"comment", "list_sub_list"}, {"explain", new List() {
                                new Map() { {"name", ""}, {"type", "tuple"}, {"comment": ""}, {"explain": new List() {
                                    new Map() { {"name", "listSubListI16"}, {"type", "i16"}, {"comment", "list_sub_list_i16"}, {"explain", new List()} },
                                    new Map() { {"name", "listSubListBst"}, {"type", "bst"}, {"comment", "list_sub_list_bst"}, {"explain", new List()} }
                                }}}
                            }}},
                            new Map() { {"name", "listSubListSingle"}, {"type", "list"}, {"comment", "list_sub_list_single"}, {"explain", 
                                new Map() { {"name", "listSubListSingleBool"}, {"type", "bool"}, {"comment", "list_sub_list_single_bool"}, {"explain", new List()} }
                            }}
                        }}}
                    }}},
                    new Map() { {"name", "keyList"}, {"type", "map"}, {"comment", "key_list"}, {"key", "listU8"}, {"explain", new List() {
                        new Map() { {"name", ""}, {"type", "tuple"}, {"comment": ""}, {"explain": new List() {
                            new Map() { {"name", "listBinary"}, {"type", "binary"}, {"comment", "list_binary"}, {"explain", 6} },
                            new Map() { {"name", "listBoolean"}, {"type", "bool"}, {"comment", "list_boolean"}, {"explain", new List()} },
                            new Map() { {"name", "listU8"}, {"type", "u8"}, {"comment", "list_u8"}, {"explain", new List()} },
                            new Map() { {"name", "listU16"}, {"type", "u16"}, {"comment", "list_u16"}, {"explain", new List()} },
                            new Map() { {"name", "listU32"}, {"type", "u32"}, {"comment", "list_u32"}, {"explain", new List()} },
                            new Map() { {"name", "listU64"}, {"type", "u64"}, {"comment", "list_u64"}, {"explain", new List()} },
                            new Map() { {"name", "listI8"}, {"type", "i8"}, {"comment", "list_i8"}, {"explain", new List()} },
                            new Map() { {"name", "listI16"}, {"type", "i16"}, {"comment", "list_i16"}, {"explain", new List()} },
                            new Map() { {"name", "listI32"}, {"type", "i32"}, {"comment", "list_i32"}, {"explain", new List()} },
                            new Map() { {"name", "listI64"}, {"type", "i64"}, {"comment", "list_i64"}, {"explain", new List()} },
                            new Map() { {"name", "listF32"}, {"type", "f32"}, {"comment", "list_f32"}, {"explain", new List()} },
                            new Map() { {"name", "listF64"}, {"type", "f64"}, {"comment", "list_f64"}, {"explain", new List()} },
                            new Map() { {"name", "listStr"}, {"type", "str"}, {"comment", "list_str"}, {"explain", new List()} },
                            new Map() { {"name", "listBst"}, {"type", "bst"}, {"comment", "list_bst"}, {"explain", new List()} }
                        }}}
                    }}}
                }}
            }}
        };
    }
}