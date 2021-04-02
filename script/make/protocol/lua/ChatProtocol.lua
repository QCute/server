local chatProtocol = {
    ["write"] = {
        [11602] = {
            {name = "list", type = "list", comment = "", explain = {
                {name = "id", type = "u64", comment = "ID", explain = {}},
                {name = "roleId", type = "u64", comment = "角色ID", explain = {}},
                {name = "roleName", type = "bst", comment = "角色名字", explain = {}},
                {name = "type", type = "u8", comment = "类型", explain = {}},
                {name = "message", type = "bst", comment = "消息内容", explain = {}}
            }}
        },
        [11603] = {
            {name = "result", type = "rst", comment = "结果", explain = {}},
            {name = "id", type = "u64", comment = "ID", explain = {}},
            {name = "roleId", type = "u64", comment = "角色ID", explain = {}},
            {name = "roleName", type = "bst", comment = "角色名字", explain = {}},
            {name = "type", type = "u8", comment = "类型", explain = {}},
            {name = "message", type = "bst", comment = "消息内容", explain = {}}
        },
        [11604] = {
            {name = "list", type = "list", comment = "", explain = {
                {name = "id", type = "u64", comment = "ID", explain = {}},
                {name = "roleId", type = "u64", comment = "角色ID", explain = {}},
                {name = "roleName", type = "bst", comment = "角色名字", explain = {}},
                {name = "type", type = "u8", comment = "类型", explain = {}},
                {name = "message", type = "bst", comment = "消息内容", explain = {}}
            }}
        },
        [11605] = {
            {name = "result", type = "rst", comment = "结果", explain = {}},
            {name = "id", type = "u64", comment = "ID", explain = {}},
            {name = "roleId", type = "u64", comment = "角色ID", explain = {}},
            {name = "roleName", type = "bst", comment = "角色名字", explain = {}},
            {name = "type", type = "u8", comment = "类型", explain = {}},
            {name = "message", type = "bst", comment = "消息内容", explain = {}}
        },
        [11606] = {
            {name = "list", type = "list", comment = "", explain = {
                {name = "id", type = "u64", comment = "ID", explain = {}},
                {name = "roleId", type = "u64", comment = "角色ID", explain = {}},
                {name = "roleName", type = "bst", comment = "角色名字", explain = {}},
                {name = "type", type = "u8", comment = "类型", explain = {}},
                {name = "message", type = "bst", comment = "消息内容", explain = {}}
            }}
        },
        [11607] = {
            {name = "result", type = "rst", comment = "结果", explain = {}},
            {name = "senderId", type = "u64", comment = "发送者角色ID", explain = {}},
            {name = "receiverId", type = "u64", comment = "接收者角色ID", explain = {}},
            {name = "type", type = "u8", comment = "类型", explain = {}},
            {name = "message", type = "bst", comment = "消息内容", explain = {}}
        },
        [11608] = {
            {name = "list", type = "list", comment = "", explain = {
                {name = "senderId", type = "u64", comment = "发送者角色ID", explain = {}},
                {name = "receiverId", type = "u64", comment = "接收者角色ID", explain = {}},
                {name = "type", type = "u8", comment = "类型", explain = {}},
                {name = "message", type = "bst", comment = "消息内容", explain = {}}
            }}
        }
    },
    ["read"] = {
        [11602] = {
            {name = "page", type = "u16", comment = "页", explain = {}}
        },
        [11603] = {
            {name = "type", type = "u8", comment = "类型", explain = {}},
            {name = "message", type = "bst", comment = "消息", explain = {}}
        },
        [11604] = {
            {name = "page", type = "u16", comment = "页", explain = {}}
        },
        [11605] = {
            {name = "type", type = "u8", comment = "类型", explain = {}},
            {name = "message", type = "bst", comment = "消息", explain = {}}
        },
        [11606] = {
            {name = "page", type = "u16", comment = "页", explain = {}}
        },
        [11607] = {
            {name = "roleId", type = "u64", comment = "角色ID", explain = {}},
            {name = "type", type = "u8", comment = "类型", explain = {}},
            {name = "message", type = "bst", comment = "消息", explain = {}}
        },
        [11608] = {
            {name = "roleId", type = "u64", comment = "角色ID", explain = {}},
            {name = "page", type = "u16", comment = "页", explain = {}}
        }
    }
}