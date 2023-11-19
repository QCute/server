export default {
    "10000": {
        "comment": "心跳包",
        "write": {"name": "data", "type": "map", "comment": "", "explain": [

        ]},
        "read": {"name": "data", "type": "ast", "comment": "结果", "explain": []}
    },
    "10001": {
        "comment": "查询账户",
        "write": {"name": "data", "type": "map", "comment": "", "explain": [
            {"name": "serverId", "type": "u16", "comment": "服务器ID", "explain": []},
            {"name": "accountName", "type": "bst", "comment": "账户名", "explain": []}
        ]},
        "read": {"name": "data", "type": "map", "comment": "", "explain": [
            {"name": "result", "type": "ast", "comment": "结果", "explain": []},
            {"name": "list", "type": "list", "comment": "角色名列表", "explain": [
                {"name": "data", "type": "map", "comment": "", "explain": [
                    {"name": "roleId", "type": "u64", "comment": "角色ID", "explain": []},
                    {"name": "roleName", "type": "bst", "comment": "角色名", "explain": []}
                ]}
            ]}
        ]}
    },
    "10002": {
        "comment": "创建账户",
        "write": {"name": "data", "type": "map", "comment": "", "explain": [
            {"name": "roleName", "type": "bst", "comment": "角色名", "explain": []},
            {"name": "serverId", "type": "u16", "comment": "服务器ID", "explain": []},
            {"name": "accountName", "type": "bst", "comment": "账户名", "explain": []},
            {"name": "sex", "type": "u8", "comment": "性别", "explain": []},
            {"name": "classes", "type": "u8", "comment": "职业", "explain": []},
            {"name": "channel", "type": "bst", "comment": "渠道", "explain": []},
            {"name": "deviceId", "type": "bst", "comment": "设备", "explain": []},
            {"name": "mac", "type": "bst", "comment": "mac地址", "explain": []},
            {"name": "deviceType", "type": "bst", "comment": "设备类型", "explain": []}
        ]},
        "read": {"name": "data", "type": "map", "comment": "", "explain": [
            {"name": "result", "type": "ast", "comment": "结果", "explain": []},
            {"name": "roleId", "type": "u64", "comment": "角色ID", "explain": []},
            {"name": "roleName", "type": "bst", "comment": "角色名", "explain": []}
        ]}
    },
    "10003": {
        "comment": "登录",
        "write": {"name": "data", "type": "map", "comment": "", "explain": [
            {"name": "roleId", "type": "u64", "comment": "角色ID", "explain": []},
            {"name": "roleName", "type": "bst", "comment": "角色名", "explain": []},
            {"name": "serverId", "type": "u16", "comment": "服务器ID", "explain": []},
            {"name": "accountName", "type": "bst", "comment": "账户名", "explain": []}
        ]},
        "read": {"name": "data", "type": "ast", "comment": "结果", "explain": []}
    },
    "10004": {
        "comment": "退出",
        "write": {"name": "data", "type": "map", "comment": "", "explain": [

        ]},
        "read": {"name": "data", "type": "ast", "comment": "结果", "explain": []}
    }
};