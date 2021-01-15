const accountProtocol = {
    "write" : {
        "10000" : [],
        "10001" : [
            {"name" : "serverId", "type" : "u16", "comment" : "服务器ID", "explain" : []},
            {"name" : "accountName", "type" : "qst", "comment" : "账户名", "explain" : []}
        ],
        "10002" : [
            {"name" : "roleName", "type" : "qst", "comment" : "角色名", "explain" : []},
            {"name" : "serverId", "type" : "u16", "comment" : "服务器ID", "explain" : []},
            {"name" : "accountName", "type" : "qst", "comment" : "账户名", "explain" : []},
            {"name" : "sex", "type" : "u8", "comment" : "性别", "explain" : []},
            {"name" : "classes", "type" : "u8", "comment" : "职业", "explain" : []},
            {"name" : "channel", "type" : "qst", "comment" : "渠道", "explain" : []},
            {"name" : "deviceId", "type" : "qst", "comment" : "设备", "explain" : []},
            {"name" : "mac", "type" : "qst", "comment" : "mac地址", "explain" : []},
            {"name" : "deviceType", "type" : "qst", "comment" : "设备类型", "explain" : []}
        ],
        "10003" : [
            {"name" : "roleId", "type" : "u64", "comment" : "角色ID", "explain" : []},
            {"name" : "roleName", "type" : "qst", "comment" : "角色名", "explain" : []},
            {"name" : "serverId", "type" : "u16", "comment" : "服务器ID", "explain" : []},
            {"name" : "accountName", "type" : "qst", "comment" : "账户名", "explain" : []}
        ],
        "10004" : []
    },
    "read" : {
        "10000" : [],
        "10001" : [
            {"name" : "list", "type" : "list", "comment" : "角色名列表", "explain" : [
                {"name" : "roleId", "type" : "u64", "comment" : "角色ID", "explain" : []},
                {"name" : "roleName", "type" : "bst", "comment" : "角色名", "explain" : []}
            ]}
        ],
        "10002" : [
            {"name" : "result", "type" : "rst", "comment" : "结果", "explain" : []},
            {"name" : "roleId", "type" : "u64", "comment" : "角色ID", "explain" : []},
            {"name" : "roleName", "type" : "bst", "comment" : "角色名", "explain" : []}
        ],
        "10003" : [
            {"name" : "result", "type" : "rst", "comment" : "结果", "explain" : []}
        ],
        "10004" : [
            {"name" : "result", "type" : "rst", "comment" : "结果", "explain" : []}
        ]
    }
};