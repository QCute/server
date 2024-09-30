export default {
    "65535" : {
        "comment" : "协议测试",
        "write" : [
            {"name": "", "type": "tuple", "comment": "", "explain": [
                {"name": "bin", "type": "binary", "comment": "bin", "explain": 6},
                {"name": "bool", "type": "bool", "comment": "bool", "explain": []},
                {"name": "u8", "type": "u8", "comment": "u8", "explain": []},
                {"name": "u16", "type": "u16", "comment": "u16", "explain": []},
                {"name": "u32", "type": "u32", "comment": "u32", "explain": []},
                {"name": "u64", "type": "u64", "comment": "u64", "explain": []},
                {"name": "i8", "type": "i8", "comment": "i8", "explain": []},
                {"name": "i16", "type": "i16", "comment": "i16", "explain": []},
                {"name": "i32", "type": "i32", "comment": "i32", "explain": []},
                {"name": "i64", "type": "i64", "comment": "i16", "explain": []},
                {"name": "f32", "type": "f32", "comment": "f32", "explain": []},
                {"name": "f64", "type": "f64", "comment": "f64", "explain": []},
                {"name": "str", "type": "str", "comment": "str", "explain": []},
                {"name": "bst", "type": "bst", "comment": "bst", "explain": []},
                {"name": "tuple", "type": "tuple", "comment": "tuple", "explain": [
                    {"name": "bin", "type": "binary", "comment": "tuple bin", "explain": 6},
                    {"name": "sub", "type": "tuple", "comment": "tuple tuple", "explain": [
                        {"name": "u8", "type": "u8", "comment": "tuple tuple u8", "explain": []},
                        {"name": "str", "type": "str", "comment": "tuple tuple str", "explain": []}
                    ]},
                    {"name": "list", "type": "list", "comment": "tuple list", "explain": [
                        {"name": "", "type": "tuple", "comment": "", "explain": [
                            {"name": "i16", "type": "i16", "comment": "tuple list i16", "explain": []},
                            {"name": "bst", "type": "bst", "comment": "tuple list bst", "explain": []}
                        ]}
                    ]},
                    {"name": "single", "type": "list", "comment": "u8", "explain": 
                        {"name": "", "type": "u8", "comment": "", "explain": []}
                    }
                ]},
                {"name": "indexList", "type": "list", "comment": "list", "explain": [
                    {"name": "", "type": "tuple", "comment": "", "explain": [
                        {"name": "bin", "type": "binary", "comment": "tuple bin", "explain": 6},
                        {"name": "sub", "type": "tuple", "comment": "tuple tuple", "explain": [
                            {"name": "u8", "type": "u8", "comment": "tuple tuple u8", "explain": []},
                            {"name": "str", "type": "str", "comment": "tuple tuple str", "explain": []}
                        ]},
                        {"name": "list", "type": "list", "comment": "tuple list", "explain": [
                            {"name": "", "type": "tuple", "comment": "", "explain": [
                                {"name": "i16", "type": "i16", "comment": "tuple list i16", "explain": []},
                                {"name": "bst", "type": "bst", "comment": "tuple list bst", "explain": []}
                            ]}
                        ]},
                        {"name": "single", "type": "list", "comment": "u8", "explain": 
                            {"name": "", "type": "u8", "comment": "", "explain": []}
                        }
                    ]}
                ]},
                {"name": "keyList", "type": "map", "comment": "", "key": "u8", "explain": [
                    {"name": "", "type": "tuple", "comment": "", "explain": [
                        {"name": "bin", "type": "binary", "comment": "bin", "explain": 6},
                        {"name": "bool", "type": "bool", "comment": "bool", "explain": []},
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
                        {"name": "bst", "type": "bst", "comment": "bst", "explain": []}
                    ]}
                ]}
            ]}
        ],
        "read" : [
            {"name": "", "type": "rst", "comment": "", "explain": []}
        ]
    }
};