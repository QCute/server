return {
    [65531] = {
        ["comment"] = "协议测试单个",
        ["write"] = {name = "data", type = "i16", comment = "single i16", explain = {}},
        ["read"] = {name = "data", type = "i16", comment = "single i16", explain = {}}
    },
    [65532] = {
        ["comment"] = "协议测试单个列表",
        ["write"] = {name = "data", type = "list", comment = "single list", explain = {
            {name = "data", type = "u32", comment = "single u32", explain = {}}
        }},
        ["read"] = {name = "data", type = "list", comment = "single list", explain = {
            {name = "data", type = "u32", comment = "single u32", explain = {}}
        }}
    },
    [65533] = {
        ["comment"] = "协议测试单个列表",
        ["write"] = {name = "data", type = "list", comment = "single list", explain = {
            {name = "data", type = "map", comment = "", explain = {
                {name = "id", type = "u64", comment = "single u64", explain = {}}
            }}
        }},
        ["read"] = {name = "data", type = "list", comment = "single list", explain = {
            {name = "data", type = "map", comment = "", explain = {
                {name = "name", type = "ast", comment = "single ast", explain = {}}
            }}
        }}
    },
    [65534] = {
        ["comment"] = "协议测试单个列表",
        ["write"] = {name = "data", type = "map", comment = "", explain = {
            {name = "id", type = "u32", comment = "single u32", explain = {}},
            {name = "theItemData", type = "map", comment = "", explain = {
                {name = "itemId", type = "u32", comment = "", explain = {}},
                {name = "type", type = "u16", comment = "single u16", explain = {}}
            }},
            {name = "name", type = "map", comment = "", explain = {
                {name = "ls", type = "list", comment = "", explain = {
                    {name = "data", type = "map", comment = "", explain = {
                        {name = "ix", type = "u32", comment = "", explain = {}},
                        {name = "nx", type = "bst", comment = "", explain = {}},
                        {name = "rx", type = "list", comment = "", explain = {
                            {name = "data", type = "u32", comment = "a list u32", explain = {}}
                        }},
                        {name = "sx", type = "list", comment = "", explain = {
                            {name = "data", type = "map", comment = "", explain = {
                                {name = "ii", type = "u32", comment = "a list ii u32", explain = {}},
                                {name = "nn", type = "bst", comment = "a list nn bst", explain = {}}
                            }}
                        }}
                    }}
                }},
                {name = "lss", type = "list", comment = "", explain = {
                    {name = "data", type = "u8", comment = "single u8", explain = {}}
                }}
            }}
        }},
        ["read"] = {name = "data", type = "list", comment = "single list", explain = {
            {name = "data", type = "map", comment = "", explain = {
                {name = "name", type = "u32", comment = "single u32", explain = {}}
            }}
        }}
    },
    [65535] = {
        ["comment"] = "协议测试",
        ["write"] = {name = "data", type = "map", comment = "", explain = {
            {name = "binary", type = "binary", comment = "binary", explain = 6},
            {name = "boolean", type = "bool", comment = "bool", explain = {}},
            {name = "u8", type = "u8", comment = "u8", explain = {}},
            {name = "u16", type = "u16", comment = "u16", explain = {}},
            {name = "u32", type = "u32", comment = "u32", explain = {}},
            {name = "u64", type = "u64", comment = "u64", explain = {}},
            {name = "i8", type = "i8", comment = "i8", explain = {}},
            {name = "i16", type = "i16", comment = "i16", explain = {}},
            {name = "i32", type = "i32", comment = "i32", explain = {}},
            {name = "i64", type = "i64", comment = "i16", explain = {}},
            {name = "f32", type = "f32", comment = "f32", explain = {}},
            {name = "f64", type = "f64", comment = "f64", explain = {}},
            {name = "str", type = "str", comment = "str", explain = {}},
            {name = "bst", type = "bst", comment = "bst", explain = {}},
            {name = "tuple", type = "map", comment = "tuple", explain = {
                {name = "binary", type = "binary", comment = "tuple binary", explain = 6},
                {name = "sub", type = "map", comment = "tuple tuple", explain = {
                    {name = "u8", type = "u8", comment = "tuple tuple u8", explain = {}},
                    {name = "str", type = "str", comment = "tuple tuple str", explain = {}}
                }},
                {name = "list", type = "list", comment = "tuple list", explain = {
                    {name = "data", type = "map", comment = "", explain = {
                        {name = "i16", type = "i16", comment = "tuple list i16", explain = {}},
                        {name = "bst", type = "bst", comment = "tuple list bst", explain = {}}
                    }}
                }},
                {name = "single", type = "list", comment = "", explain = {
                    {name = "data", type = "bool", comment = "bool", explain = {}}
                }}
            }},
            {name = "indexList", type = "list", comment = "list", explain = {
                {name = "data", type = "map", comment = "", explain = {
                    {name = "binary", type = "binary", comment = "tuple binary", explain = 6},
                    {name = "sub", type = "map", comment = "tuple tuple", explain = {
                        {name = "u8", type = "u8", comment = "tuple tuple u8", explain = {}},
                        {name = "str", type = "str", comment = "tuple tuple str", explain = {}}
                    }},
                    {name = "list", type = "list", comment = "tuple list", explain = {
                        {name = "data", type = "map", comment = "", explain = {
                            {name = "i16", type = "i16", comment = "tuple list i16", explain = {}},
                            {name = "bst", type = "bst", comment = "tuple list bst", explain = {}}
                        }}
                    }},
                    {name = "single", type = "list", comment = "", explain = {
                        {name = "data", type = "bool", comment = "bool", explain = {}}
                    }}
                }}
            }},
            {name = "keyList", type = "list", comment = "", key = "u8", explain = {
                {name = "data", type = "map", comment = "", explain = {
                    {name = "binary", type = "binary", comment = "binary", explain = 6},
                    {name = "boolean", type = "bool", comment = "bool", explain = {}},
                    {name = "u8", type = "u8", comment = "u8", explain = {}},
                    {name = "u16", type = "u16", comment = "u16", explain = {}},
                    {name = "u32", type = "u32", comment = "u32", explain = {}},
                    {name = "u64", type = "u64", comment = "u64", explain = {}},
                    {name = "i8", type = "i8", comment = "i8", explain = {}},
                    {name = "i16", type = "i16", comment = "i16", explain = {}},
                    {name = "i32", type = "i32", comment = "i32", explain = {}},
                    {name = "i64", type = "i64", comment = "i64", explain = {}},
                    {name = "f32", type = "f32", comment = "f32", explain = {}},
                    {name = "f64", type = "f64", comment = "f64", explain = {}},
                    {name = "str", type = "str", comment = "str", explain = {}},
                    {name = "bst", type = "bst", comment = "bst", explain = {}}
                }}
            }}
        }},
        ["read"] = {name = "data", type = "map", comment = "", explain = {
            {name = "binary", type = "binary", comment = "binary", explain = 6},
            {name = "boolean", type = "bool", comment = "bool", explain = {}},
            {name = "u8", type = "u8", comment = "u8", explain = {}},
            {name = "u16", type = "u16", comment = "u16", explain = {}},
            {name = "u32", type = "u32", comment = "u32", explain = {}},
            {name = "u64", type = "u64", comment = "u64", explain = {}},
            {name = "i8", type = "i8", comment = "i8", explain = {}},
            {name = "i16", type = "i16", comment = "i16", explain = {}},
            {name = "i32", type = "i32", comment = "i32", explain = {}},
            {name = "i64", type = "i64", comment = "i16", explain = {}},
            {name = "f32", type = "f32", comment = "f32", explain = {}},
            {name = "f64", type = "f64", comment = "f64", explain = {}},
            {name = "str", type = "str", comment = "str", explain = {}},
            {name = "bst", type = "bst", comment = "bst", explain = {}},
            {name = "tuple", type = "map", comment = "tuple", explain = {
                {name = "binary", type = "binary", comment = "tuple binary", explain = 6},
                {name = "sub", type = "map", comment = "tuple tuple", explain = {
                    {name = "u8", type = "u8", comment = "tuple tuple u8", explain = {}},
                    {name = "str", type = "str", comment = "tuple tuple str", explain = {}}
                }},
                {name = "list", type = "list", comment = "tuple list", explain = {
                    {name = "data", type = "map", comment = "", explain = {
                        {name = "i16", type = "i16", comment = "tuple list i16", explain = {}},
                        {name = "bst", type = "bst", comment = "tuple list bst", explain = {}}
                    }}
                }},
                {name = "single", type = "list", comment = "", explain = {
                    {name = "data", type = "bool", comment = "bool", explain = {}}
                }}
            }},
            {name = "indexList", type = "list", comment = "list", explain = {
                {name = "data", type = "map", comment = "", explain = {
                    {name = "binary", type = "binary", comment = "tuple binary", explain = 6},
                    {name = "sub", type = "map", comment = "tuple tuple", explain = {
                        {name = "u8", type = "u8", comment = "tuple tuple u8", explain = {}},
                        {name = "str", type = "str", comment = "tuple tuple str", explain = {}}
                    }},
                    {name = "list", type = "list", comment = "tuple list", explain = {
                        {name = "data", type = "map", comment = "", explain = {
                            {name = "i16", type = "i16", comment = "tuple list i16", explain = {}},
                            {name = "bst", type = "bst", comment = "tuple list bst", explain = {}}
                        }}
                    }},
                    {name = "single", type = "list", comment = "", explain = {
                        {name = "data", type = "bool", comment = "bool", explain = {}}
                    }}
                }}
            }},
            {name = "keyList", type = "list", comment = "", key = "u8", explain = {
                {name = "data", type = "map", comment = "", explain = {
                    {name = "binary", type = "binary", comment = "binary", explain = 6},
                    {name = "boolean", type = "bool", comment = "boolean", explain = {}},
                    {name = "u8", type = "u8", comment = "u8", explain = {}},
                    {name = "u16", type = "u16", comment = "u16", explain = {}},
                    {name = "u32", type = "u32", comment = "u32", explain = {}},
                    {name = "u64", type = "u64", comment = "u64", explain = {}},
                    {name = "i8", type = "i8", comment = "i8", explain = {}},
                    {name = "i16", type = "i16", comment = "i16", explain = {}},
                    {name = "i32", type = "i32", comment = "i32", explain = {}},
                    {name = "i64", type = "i64", comment = "i64", explain = {}},
                    {name = "f32", type = "f32", comment = "f32", explain = {}},
                    {name = "f64", type = "f64", comment = "f64", explain = {}},
                    {name = "str", type = "str", comment = "str", explain = {}},
                    {name = "bst", type = "bst", comment = "bst", explain = {}}
                }}
            }}
        }}
    }
}