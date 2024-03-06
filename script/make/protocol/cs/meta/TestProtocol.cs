using List = System.Collections.Generic.List<System.Object>;
using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;

public static class TestProtocol
{
    public static Map GetMeta()
    {
        return new Map()
        {
            {"65532", new Map() {
                {"comment", "协议测试单个"},
                {"write", new Map() { {"name", "data"}, {"type", "i16"}, {"comment", "single i16"}, {"explain", new List()} }},
                {"read", new Map() { {"name", "data"}, {"type", "i16"}, {"comment", "single i16"}, {"explain", new List()} }}
            }},
            {"65533", new Map() {
                {"comment", "协议测试单个列表"},
                {"write", new Map() { {"name", "data"}, {"type", "list"}, {"comment", "single list"}, {"explain", new List() {
                    new Map() { {"name", "item"}, {"type", "u32"}, {"comment", "single u32"}, {"explain", new List()} }
                }}}},
                {"read", new Map() { {"name", "data"}, {"type", "list"}, {"comment", "single list"}, {"explain", new List() {
                    new Map() { {"name", "item"}, {"type", "u32"}, {"comment", "single u32"}, {"explain", new List()} }
                }}}}
            }},
            {"65534", new Map() {
                {"comment", "协议测试单个键值列表"},
                {"write", new Map() { {"name", "data"}, {"type", "list"}, {"comment", "key single list"}, {"key", "u32"}, {"explain", new List() {
                    new Map() { {"name", "item"}, {"type", "u32"}, {"comment", "key single u32"}, {"explain", new List()} }
                }}}},
                {"read", new Map() { {"name", "data"}, {"type", "list"}, {"comment", "key single list"}, {"key", "u32"}, {"explain", new List() {
                    new Map() { {"name", "item"}, {"type", "u32"}, {"comment", "key single u32"}, {"explain", new List()} }
                }}}}
            }},
            {"65535", new Map() {
                {"comment", "协议测试"},
                {"write", new Map() { {"name", "data"}, {"type", "map"}, {"comment": ""}, {"explain": new List() {
                    new Map() { {"name", "binary"}, {"type", "binary"}, {"comment", "binary"}, {"explain", 6} },
                    new Map() { {"name", "boolean"}, {"type", "bool"}, {"comment", "bool"}, {"explain", new List()} },
                    new Map() { {"name", "u8"}, {"type", "u8"}, {"comment", "u8"}, {"explain", new List()} },
                    new Map() { {"name", "u16"}, {"type", "u16"}, {"comment", "u16"}, {"explain", new List()} },
                    new Map() { {"name", "u32"}, {"type", "u32"}, {"comment", "u32"}, {"explain", new List()} },
                    new Map() { {"name", "u64"}, {"type", "u64"}, {"comment", "u64"}, {"explain", new List()} },
                    new Map() { {"name", "i8"}, {"type", "i8"}, {"comment", "i8"}, {"explain", new List()} },
                    new Map() { {"name", "i16"}, {"type", "i16"}, {"comment", "i16"}, {"explain", new List()} },
                    new Map() { {"name", "i32"}, {"type", "i32"}, {"comment", "i32"}, {"explain", new List()} },
                    new Map() { {"name", "i64"}, {"type", "i64"}, {"comment", "i16"}, {"explain", new List()} },
                    new Map() { {"name", "f32"}, {"type", "f32"}, {"comment", "f32"}, {"explain", new List()} },
                    new Map() { {"name", "f64"}, {"type", "f64"}, {"comment", "f64"}, {"explain", new List()} },
                    new Map() { {"name", "str"}, {"type", "str"}, {"comment", "str"}, {"explain", new List()} },
                    new Map() { {"name", "bst"}, {"type", "bst"}, {"comment", "bst"}, {"explain", new List()} },
                    new Map() { {"name", "tuple"}, {"type", "map"}, {"comment": "tuple"}, {"explain": new List() {
                        new Map() { {"name", "binary"}, {"type", "binary"}, {"comment", "tuple binary"}, {"explain", 6} },
                        new Map() { {"name", "sub"}, {"type", "map"}, {"comment": "tuple tuple"}, {"explain": new List() {
                            new Map() { {"name", "u8"}, {"type", "u8"}, {"comment", "tuple tuple u8"}, {"explain", new List()} },
                            new Map() { {"name", "str"}, {"type", "str"}, {"comment", "tuple tuple str"}, {"explain", new List()} }
                        }}},
                        new Map() { {"name", "list"}, {"type", "list"}, {"comment", "tuple list"}, {"explain", new List() {
                            new Map() { {"name", "item"}, {"type", "map"}, {"comment": ""}, {"explain": new List() {
                                new Map() { {"name", "i16"}, {"type", "i16"}, {"comment", "tuple list i16"}, {"explain", new List()} },
                                new Map() { {"name", "bst"}, {"type", "bst"}, {"comment", "tuple list bst"}, {"explain", new List()} }
                            }}}
                        }}},
                        new Map() { {"name", "single"}, {"type", "list"}, {"comment", ""}, {"explain", new List() {
                            new Map() { {"name", "item"}, {"type", "bool"}, {"comment", "bool"}, {"explain", new List()} }
                        }}}
                    }}},
                    new Map() { {"name", "indexList"}, {"type", "list"}, {"comment", "list"}, {"explain", new List() {
                        new Map() { {"name", "item"}, {"type", "map"}, {"comment": ""}, {"explain": new List() {
                            new Map() { {"name", "binary"}, {"type", "binary"}, {"comment", "tuple binary"}, {"explain", 6} },
                            new Map() { {"name", "sub"}, {"type", "map"}, {"comment": "tuple tuple"}, {"explain": new List() {
                                new Map() { {"name", "u8"}, {"type", "u8"}, {"comment", "tuple tuple u8"}, {"explain", new List()} },
                                new Map() { {"name", "str"}, {"type", "str"}, {"comment", "tuple tuple str"}, {"explain", new List()} }
                            }}},
                            new Map() { {"name", "list"}, {"type", "list"}, {"comment", "tuple list"}, {"explain", new List() {
                                new Map() { {"name", "item"}, {"type", "map"}, {"comment": ""}, {"explain": new List() {
                                    new Map() { {"name", "i16"}, {"type", "i16"}, {"comment", "tuple list i16"}, {"explain", new List()} },
                                    new Map() { {"name", "bst"}, {"type", "bst"}, {"comment", "tuple list bst"}, {"explain", new List()} }
                                }}}
                            }}},
                            new Map() { {"name", "single"}, {"type", "list"}, {"comment", ""}, {"explain", new List() {
                                new Map() { {"name", "item"}, {"type", "bool"}, {"comment", "bool"}, {"explain", new List()} }
                            }}}
                        }}}
                    }}},
                    new Map() { {"name", "keyList"}, {"type", "list"}, {"comment", ""}, {"key", "u8"}, {"explain", new List() {
                        new Map() { {"name", "item"}, {"type", "map"}, {"comment": ""}, {"explain": new List() {
                            new Map() { {"name", "binary"}, {"type", "binary"}, {"comment", "binary"}, {"explain", 6} },
                            new Map() { {"name", "boolean"}, {"type", "bool"}, {"comment", "bool"}, {"explain", new List()} },
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
                            new Map() { {"name", "bst"}, {"type", "bst"}, {"comment", "bst"}, {"explain", new List()} }
                        }}}
                    }}}
                }}}},
                {"read", new Map() { {"name", "data"}, {"type", "map"}, {"comment": ""}, {"explain": new List() {
                    new Map() { {"name", "binary"}, {"type", "binary"}, {"comment", "binary"}, {"explain", 6} },
                    new Map() { {"name", "boolean"}, {"type", "bool"}, {"comment", "bool"}, {"explain", new List()} },
                    new Map() { {"name", "u8"}, {"type", "u8"}, {"comment", "u8"}, {"explain", new List()} },
                    new Map() { {"name", "u16"}, {"type", "u16"}, {"comment", "u16"}, {"explain", new List()} },
                    new Map() { {"name", "u32"}, {"type", "u32"}, {"comment", "u32"}, {"explain", new List()} },
                    new Map() { {"name", "u64"}, {"type", "u64"}, {"comment", "u64"}, {"explain", new List()} },
                    new Map() { {"name", "i8"}, {"type", "i8"}, {"comment", "i8"}, {"explain", new List()} },
                    new Map() { {"name", "i16"}, {"type", "i16"}, {"comment", "i16"}, {"explain", new List()} },
                    new Map() { {"name", "i32"}, {"type", "i32"}, {"comment", "i32"}, {"explain", new List()} },
                    new Map() { {"name", "i64"}, {"type", "i64"}, {"comment", "i16"}, {"explain", new List()} },
                    new Map() { {"name", "f32"}, {"type", "f32"}, {"comment", "f32"}, {"explain", new List()} },
                    new Map() { {"name", "f64"}, {"type", "f64"}, {"comment", "f64"}, {"explain", new List()} },
                    new Map() { {"name", "str"}, {"type", "str"}, {"comment", "str"}, {"explain", new List()} },
                    new Map() { {"name", "bst"}, {"type", "bst"}, {"comment", "bst"}, {"explain", new List()} },
                    new Map() { {"name", "tuple"}, {"type", "map"}, {"comment": "tuple"}, {"explain": new List() {
                        new Map() { {"name", "binary"}, {"type", "binary"}, {"comment", "tuple binary"}, {"explain", 6} },
                        new Map() { {"name", "sub"}, {"type", "map"}, {"comment": "tuple tuple"}, {"explain": new List() {
                            new Map() { {"name", "u8"}, {"type", "u8"}, {"comment", "tuple tuple u8"}, {"explain", new List()} },
                            new Map() { {"name", "str"}, {"type", "str"}, {"comment", "tuple tuple str"}, {"explain", new List()} }
                        }}},
                        new Map() { {"name", "list"}, {"type", "list"}, {"comment", "tuple list"}, {"explain", new List() {
                            new Map() { {"name", "item"}, {"type", "map"}, {"comment": ""}, {"explain": new List() {
                                new Map() { {"name", "i16"}, {"type", "i16"}, {"comment", "tuple list i16"}, {"explain", new List()} },
                                new Map() { {"name", "bst"}, {"type", "bst"}, {"comment", "tuple list bst"}, {"explain", new List()} }
                            }}}
                        }}},
                        new Map() { {"name", "single"}, {"type", "list"}, {"comment", ""}, {"explain", new List() {
                            new Map() { {"name", "item"}, {"type", "bool"}, {"comment", "bool"}, {"explain", new List()} }
                        }}}
                    }}},
                    new Map() { {"name", "indexList"}, {"type", "list"}, {"comment", "list"}, {"explain", new List() {
                        new Map() { {"name", "item"}, {"type", "map"}, {"comment": ""}, {"explain": new List() {
                            new Map() { {"name", "binary"}, {"type", "binary"}, {"comment", "tuple binary"}, {"explain", 6} },
                            new Map() { {"name", "sub"}, {"type", "map"}, {"comment": "tuple tuple"}, {"explain": new List() {
                                new Map() { {"name", "u8"}, {"type", "u8"}, {"comment", "tuple tuple u8"}, {"explain", new List()} },
                                new Map() { {"name", "str"}, {"type", "str"}, {"comment", "tuple tuple str"}, {"explain", new List()} }
                            }}},
                            new Map() { {"name", "list"}, {"type", "list"}, {"comment", "tuple list"}, {"explain", new List() {
                                new Map() { {"name", "item"}, {"type", "map"}, {"comment": ""}, {"explain": new List() {
                                    new Map() { {"name", "i16"}, {"type", "i16"}, {"comment", "tuple list i16"}, {"explain", new List()} },
                                    new Map() { {"name", "bst"}, {"type", "bst"}, {"comment", "tuple list bst"}, {"explain", new List()} }
                                }}}
                            }}},
                            new Map() { {"name", "single"}, {"type", "list"}, {"comment", ""}, {"explain", new List() {
                                new Map() { {"name", "item"}, {"type", "bool"}, {"comment", "bool"}, {"explain", new List()} }
                            }}}
                        }}}
                    }}},
                    new Map() { {"name", "keyList"}, {"type", "list"}, {"comment", ""}, {"key", "u8"}, {"explain", new List() {
                        new Map() { {"name", "item"}, {"type", "map"}, {"comment": ""}, {"explain": new List() {
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
                            new Map() { {"name", "bst"}, {"type", "bst"}, {"comment", "bst"}, {"explain", new List()} }
                        }}}
                    }}}
                }}}}
            }}
        };
    }
}