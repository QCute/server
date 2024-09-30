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
                    new Map() { {"name", ""}, {"type", "tuple"}, {"comment": ""}, {"explain": new List() {
                        new Map() { {"name", "bin"}, {"type", "binary"}, {"comment", "bin"}, {"explain", 6} },
                        new Map() { {"name", "bool"}, {"type", "bool"}, {"comment", "bool"}, {"explain", new List()} },
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
                        new Map() { {"name", "tuple"}, {"type", "tuple"}, {"comment": "tuple"}, {"explain": new List() {
                            new Map() { {"name", "bin"}, {"type", "binary"}, {"comment", "tuple bin"}, {"explain", 6} },
                            new Map() { {"name", "sub"}, {"type", "tuple"}, {"comment": "tuple tuple"}, {"explain": new List() {
                                new Map() { {"name", "u8"}, {"type", "u8"}, {"comment", "tuple tuple u8"}, {"explain", new List()} },
                                new Map() { {"name", "str"}, {"type", "str"}, {"comment", "tuple tuple str"}, {"explain", new List()} }
                            }}},
                            new Map() { {"name", "list"}, {"type", "list"}, {"comment", "tuple list"}, {"explain", new List() {
                                new Map() { {"name", ""}, {"type", "tuple"}, {"comment": ""}, {"explain": new List() {
                                    new Map() { {"name", "i16"}, {"type", "i16"}, {"comment", "tuple list i16"}, {"explain", new List()} },
                                    new Map() { {"name", "bst"}, {"type", "bst"}, {"comment", "tuple list bst"}, {"explain", new List()} }
                                }}}
                            }}},
                            new Map() { {"name", "single"}, {"type", "list"}, {"comment", "u8"}, {"explain", 
                                new Map() { {"name", ""}, {"type", "u8"}, {"comment", ""}, {"explain", new List()} }
                            }}
                        }}},
                        new Map() { {"name", "indexList"}, {"type", "list"}, {"comment", "list"}, {"explain", new List() {
                            new Map() { {"name", ""}, {"type", "tuple"}, {"comment": ""}, {"explain": new List() {
                                new Map() { {"name", "bin"}, {"type", "binary"}, {"comment", "tuple bin"}, {"explain", 6} },
                                new Map() { {"name", "sub"}, {"type", "tuple"}, {"comment": "tuple tuple"}, {"explain": new List() {
                                    new Map() { {"name", "u8"}, {"type", "u8"}, {"comment", "tuple tuple u8"}, {"explain", new List()} },
                                    new Map() { {"name", "str"}, {"type", "str"}, {"comment", "tuple tuple str"}, {"explain", new List()} }
                                }}},
                                new Map() { {"name", "list"}, {"type", "list"}, {"comment", "tuple list"}, {"explain", new List() {
                                    new Map() { {"name", ""}, {"type", "tuple"}, {"comment": ""}, {"explain": new List() {
                                        new Map() { {"name", "i16"}, {"type", "i16"}, {"comment", "tuple list i16"}, {"explain", new List()} },
                                        new Map() { {"name", "bst"}, {"type", "bst"}, {"comment", "tuple list bst"}, {"explain", new List()} }
                                    }}}
                                }}},
                                new Map() { {"name", "single"}, {"type", "list"}, {"comment", "u8"}, {"explain", 
                                    new Map() { {"name", ""}, {"type", "u8"}, {"comment", ""}, {"explain", new List()} }
                                }}
                            }}}
                        }}},
                        new Map() { {"name", "keyList"}, {"type", "map"}, {"comment", ""}, {"key", "u8"}, {"explain", new List() {
                            new Map() { {"name", ""}, {"type", "tuple"}, {"comment": ""}, {"explain": new List() {
                                new Map() { {"name", "bin"}, {"type", "binary"}, {"comment", "bin"}, {"explain", 6} },
                                new Map() { {"name", "bool"}, {"type", "bool"}, {"comment", "bool"}, {"explain", new List()} },
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
                    }}}
                }},
                {"read", new List() {
                    new Map() { {"name", ""}, {"type", "rst"}, {"comment", ""}, {"explain", new List()} }
                }}
            }}
        };
    }
}