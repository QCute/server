export default {
    "11301" : {
        "comment" : "已购列表",
        "write" : [
            {"name": "data", "type": "tuple", "comment": "", "explain": [

            ]}
        ],
        "read" : [
            {"name": "data", "type": "list", "comment": "已购买列表", "explain": [
                {"name": "shop", "type": "record", "comment": "", "explain": [
                    {"name": "shopId", "type": "u32", "comment": "商店ID", "explain": []},
                    {"name": "number", "type": "u16", "comment": "数量", "explain": []}
                ]}
            ]}
        ]
    },
    "11302" : {
        "comment" : "购买",
        "write" : [
            {"name": "data", "type": "tuple", "comment": "", "explain": [
                {"name": "shopId", "type": "u32", "comment": "商店ID", "explain": []},
                {"name": "number", "type": "u16", "comment": "数量", "explain": []}
            ]}
        ],
        "read" : [
            {"name": "data", "type": "rst", "comment": "结果", "explain": []}
        ]
    }
};