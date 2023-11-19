export default {
    "10101": {
        "comment": "角色",
        "write": {"name": "data", "type": "map", "comment": "", "explain": [

        ]},
        "read": {"name": "data", "type": "map", "comment": "", "explain": [
            {"name": "roleId", "type": "u64", "comment": "角色ID", "explain": []},
            {"name": "roleName", "type": "bst", "comment": "角色名", "explain": []},
            {"name": "sex", "type": "u8", "comment": "性别", "explain": []},
            {"name": "classes", "type": "u8", "comment": "职业", "explain": []},
            {"name": "level", "type": "u64", "comment": "等级", "explain": []}
        ]}
    },
    "10102": {
        "comment": "资产",
        "write": {"name": "data", "type": "map", "comment": "", "explain": [

        ]},
        "read": {"name": "data", "type": "map", "comment": "", "explain": [
            {"name": "gold", "type": "u64", "comment": "金币", "explain": []},
            {"name": "silver", "type": "u32", "comment": "银币", "explain": []},
            {"name": "copper", "type": "u64", "comment": "铜币", "explain": []},
            {"name": "exp", "type": "u64", "comment": "经验", "explain": []}
        ]}
    },
    "10103": {
        "comment": "vip",
        "write": {"name": "data", "type": "map", "comment": "", "explain": [

        ]},
        "read": {"name": "data", "type": "map", "comment": "", "explain": [
            {"name": "vipLevel", "type": "u8", "comment": "等级", "explain": []},
            {"name": "exp", "type": "u64", "comment": "经验", "explain": []},
            {"name": "expireTime", "type": "u32", "comment": "过期时间", "explain": []}
        ]}
    }
};