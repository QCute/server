export default {
    "19001": {
        "comment": "等级榜",
        "write": {"name": "data", "type": "map", "comment": "", "explain": [

        ]},
        "read": {"name": "data", "type": "list", "comment": "", "explain": [
            {"name": "data", "type": "map", "comment": "", "explain": [
                {"name": "type", "type": "u16", "comment": "类型", "explain": []},
                {"name": "order", "type": "u64", "comment": "排名", "explain": []},
                {"name": "key", "type": "u64", "comment": "键", "explain": []},
                {"name": "value", "type": "u64", "comment": "值", "explain": []},
                {"name": "time", "type": "u32", "comment": "时间", "explain": []},
                {"name": "name", "type": "bst", "comment": "名字", "explain": []},
                {"name": "serverId", "type": "u16", "comment": "服务器ID", "explain": []}
            ]}
        ]}
    },
    "19002": {
        "comment": "战力榜",
        "write": {"name": "data", "type": "map", "comment": "", "explain": [

        ]},
        "read": {"name": "data", "type": "list", "comment": "", "explain": [
            {"name": "data", "type": "map", "comment": "", "explain": [
                {"name": "type", "type": "u16", "comment": "类型", "explain": []},
                {"name": "order", "type": "u64", "comment": "排名", "explain": []},
                {"name": "key", "type": "u64", "comment": "键", "explain": []},
                {"name": "value", "type": "u64", "comment": "值", "explain": []},
                {"name": "time", "type": "u32", "comment": "时间", "explain": []},
                {"name": "name", "type": "bst", "comment": "名字", "explain": []},
                {"name": "serverId", "type": "u16", "comment": "服务器ID", "explain": []},
                {"name": "other", "type": "map", "comment": "", "explain": [
                    {"name": "level", "type": "u16", "comment": "等级", "explain": []},
                    {"name": "classes", "type": "u8", "comment": "职业", "explain": []}
                ]}
            ]}
        ]}
    },
    "19003": {
        "comment": "成就榜",
        "write": {"name": "data", "type": "map", "comment": "", "explain": [

        ]},
        "read": {"name": "data", "type": "list", "comment": "", "explain": [
            {"name": "data", "type": "map", "comment": "", "explain": [
                {"name": "type", "type": "u16", "comment": "类型", "explain": []},
                {"name": "order", "type": "u64", "comment": "排名", "explain": []},
                {"name": "key", "type": "u64", "comment": "键", "explain": []},
                {"name": "value", "type": "u64", "comment": "值", "explain": []},
                {"name": "time", "type": "u32", "comment": "时间", "explain": []},
                {"name": "name", "type": "bst", "comment": "名字", "explain": []},
                {"name": "serverId", "type": "u16", "comment": "服务器ID", "explain": []},
                {"name": "other", "type": "map", "comment": "", "explain": [
                    {"name": "level", "type": "u16", "comment": "等级", "explain": []},
                    {"name": "classes", "type": "u8", "comment": "职业", "explain": []},
                    {"name": "sex", "type": "u8", "comment": "性别", "explain": []}
                ]}
            ]}
        ]}
    },
    "19004": {
        "comment": "财富榜",
        "write": {"name": "data", "type": "map", "comment": "", "explain": [

        ]},
        "read": {"name": "data", "type": "list", "comment": "", "explain": [
            {"name": "data", "type": "map", "comment": "", "explain": [
                {"name": "type", "type": "u16", "comment": "类型", "explain": []},
                {"name": "order", "type": "u64", "comment": "排名", "explain": []},
                {"name": "key", "type": "u64", "comment": "键", "explain": []},
                {"name": "value", "type": "u64", "comment": "值", "explain": []},
                {"name": "time", "type": "u32", "comment": "时间", "explain": []},
                {"name": "name", "type": "bst", "comment": "名字", "explain": []},
                {"name": "serverId", "type": "u16", "comment": "服务器ID", "explain": []},
                {"name": "other", "type": "map", "comment": "", "explain": [
                    {"name": "level", "type": "u16", "comment": "等级", "explain": []},
                    {"name": "classes", "type": "u8", "comment": "职业", "explain": []},
                    {"name": "sex", "type": "u8", "comment": "性别", "explain": []},
                    {"name": "vipLevel", "type": "u8", "comment": "VIP等级", "explain": []}
                ]}
            ]}
        ]}
    },
    "19005": {
        "comment": "职业榜",
        "write": {"name": "data", "type": "map", "comment": "", "explain": [

        ]},
        "read": {"name": "data", "type": "list", "comment": "", "explain": [
            {"name": "data", "type": "map", "comment": "", "explain": [
                {"name": "type", "type": "u16", "comment": "类型", "explain": []},
                {"name": "order", "type": "u64", "comment": "排名", "explain": []},
                {"name": "key", "type": "u64", "comment": "键", "explain": []},
                {"name": "value", "type": "u64", "comment": "值", "explain": []},
                {"name": "time", "type": "u32", "comment": "时间", "explain": []},
                {"name": "name", "type": "bst", "comment": "名字", "explain": []},
                {"name": "serverId", "type": "u16", "comment": "服务器ID", "explain": []},
                {"name": "other", "type": "map", "comment": "", "explain": [
                    {"name": "level", "type": "u16", "comment": "等级", "explain": []},
                    {"name": "classes", "type": "u8", "comment": "职业", "explain": []},
                    {"name": "sex", "type": "u8", "comment": "性别", "explain": []},
                    {"name": "vipLevel", "type": "u8", "comment": "VIP等级", "explain": []},
                    {"name": "avatar", "type": "u8", "comment": "头像", "explain": []}
                ]}
            ]}
        ]}
    }
};