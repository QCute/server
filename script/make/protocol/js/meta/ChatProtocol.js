export default {
    "11602": {
        "comment": "系统公告列表",
        "write": {"name": "data", "type": "u16", "comment": "页", "explain": []},
        "read": {"name": "data", "type": "list", "comment": "", "explain": [
            {"name": "data", "type": "map", "comment": "", "explain": [
                {"name": "id", "type": "u64", "comment": "ID", "explain": []},
                {"name": "roleId", "type": "u64", "comment": "角色ID", "explain": []},
                {"name": "roleName", "type": "bst", "comment": "角色名字", "explain": []},
                {"name": "type", "type": "u8", "comment": "类型", "explain": []},
                {"name": "message", "type": "bst", "comment": "消息内容", "explain": []}
            ]}
        ]}
    },
    "11603": {
        "comment": "世界聊天",
        "write": {"name": "data", "type": "map", "comment": "", "explain": [
            {"name": "type", "type": "u8", "comment": "类型", "explain": []},
            {"name": "message", "type": "bst", "comment": "消息", "explain": []}
        ]},
        "read": {"name": "data", "type": "map", "comment": "", "explain": [
            {"name": "result", "type": "ast", "comment": "结果", "explain": []},
            {"name": "worldChat", "type": "map", "comment": "", "explain": [
                {"name": "id", "type": "u64", "comment": "ID", "explain": []},
                {"name": "roleId", "type": "u64", "comment": "角色ID", "explain": []},
                {"name": "roleName", "type": "bst", "comment": "角色名字", "explain": []},
                {"name": "type", "type": "u8", "comment": "类型", "explain": []},
                {"name": "message", "type": "bst", "comment": "消息内容", "explain": []}
            ]}
        ]}
    },
    "11604": {
        "comment": "世界聊天列表",
        "write": {"name": "data", "type": "u16", "comment": "页", "explain": []},
        "read": {"name": "data", "type": "list", "comment": "", "explain": [
            {"name": "data", "type": "map", "comment": "", "explain": [
                {"name": "id", "type": "u64", "comment": "ID", "explain": []},
                {"name": "roleId", "type": "u64", "comment": "角色ID", "explain": []},
                {"name": "roleName", "type": "bst", "comment": "角色名字", "explain": []},
                {"name": "type", "type": "u8", "comment": "类型", "explain": []},
                {"name": "message", "type": "bst", "comment": "消息内容", "explain": []}
            ]}
        ]}
    },
    "11605": {
        "comment": "公会聊天",
        "write": {"name": "data", "type": "map", "comment": "", "explain": [
            {"name": "type", "type": "u8", "comment": "类型", "explain": []},
            {"name": "message", "type": "bst", "comment": "消息", "explain": []}
        ]},
        "read": {"name": "data", "type": "map", "comment": "", "explain": [
            {"name": "result", "type": "ast", "comment": "结果", "explain": []},
            {"name": "guildChat", "type": "map", "comment": "", "explain": [
                {"name": "id", "type": "u64", "comment": "ID", "explain": []},
                {"name": "roleId", "type": "u64", "comment": "角色ID", "explain": []},
                {"name": "roleName", "type": "bst", "comment": "角色名字", "explain": []},
                {"name": "type", "type": "u8", "comment": "类型", "explain": []},
                {"name": "message", "type": "bst", "comment": "消息内容", "explain": []}
            ]}
        ]}
    },
    "11606": {
        "comment": "公会聊天列表",
        "write": {"name": "data", "type": "u16", "comment": "页", "explain": []},
        "read": {"name": "data", "type": "list", "comment": "", "explain": [
            {"name": "data", "type": "map", "comment": "", "explain": [
                {"name": "id", "type": "u64", "comment": "ID", "explain": []},
                {"name": "roleId", "type": "u64", "comment": "角色ID", "explain": []},
                {"name": "roleName", "type": "bst", "comment": "角色名字", "explain": []},
                {"name": "type", "type": "u8", "comment": "类型", "explain": []},
                {"name": "message", "type": "bst", "comment": "消息内容", "explain": []}
            ]}
        ]}
    },
    "11607": {
        "comment": "私聊",
        "write": {"name": "data", "type": "map", "comment": "", "explain": [
            {"name": "roleId", "type": "u64", "comment": "角色ID", "explain": []},
            {"name": "type", "type": "u8", "comment": "类型", "explain": []},
            {"name": "message", "type": "bst", "comment": "消息", "explain": []}
        ]},
        "read": {"name": "data", "type": "map", "comment": "", "explain": [
            {"name": "result", "type": "ast", "comment": "结果", "explain": []},
            {"name": "privateChat", "type": "map", "comment": "", "explain": [
                {"name": "senderId", "type": "u64", "comment": "发送者角色ID", "explain": []},
                {"name": "receiverId", "type": "u64", "comment": "接收者角色ID", "explain": []},
                {"name": "type", "type": "u8", "comment": "类型", "explain": []},
                {"name": "message", "type": "bst", "comment": "消息内容", "explain": []}
            ]}
        ]}
    },
    "11608": {
        "comment": "私聊列表",
        "write": {"name": "data", "type": "map", "comment": "", "explain": [
            {"name": "roleId", "type": "u64", "comment": "角色ID", "explain": []},
            {"name": "page", "type": "u16", "comment": "页", "explain": []}
        ]},
        "read": {"name": "data", "type": "list", "comment": "", "explain": [
            {"name": "data", "type": "map", "comment": "", "explain": [
                {"name": "senderId", "type": "u64", "comment": "发送者角色ID", "explain": []},
                {"name": "receiverId", "type": "u64", "comment": "接收者角色ID", "explain": []},
                {"name": "type", "type": "u8", "comment": "类型", "explain": []},
                {"name": "message", "type": "bst", "comment": "消息内容", "explain": []}
            ]}
        ]}
    }
};