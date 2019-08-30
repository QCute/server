let accountProtocol = {
    "read" : {
        "10000" : [],
        "10001" : [
            {"name" : "serverId", "type" : "u16", "comment" : "服务器ID", "explain" : []},
            {"name" : "accountName", "type" : "bst", "comment" : "账户名", "explain" : []}
        ],
        "10002" : [
            {"name" : "serverId", "type" : "u16", "comment" : "服务器ID", "explain" : []},
            {"name" : "sex", "type" : "u8", "comment" : "性别", "explain" : []},
            {"name" : "career", "type" : "u8", "comment" : "职业", "explain" : []},
            {"name" : "channelId", "type" : "u16", "comment" : "渠道ID", "explain" : []},
            {"name" : "name", "type" : "bst", "comment" : "名字", "explain" : []},
            {"name" : "nick", "type" : "bst", "comment" : "昵称", "explain" : []},
            {"name" : "deviceId", "type" : "bst", "comment" : "设备", "explain" : []},
            {"name" : "mac", "type" : "bst", "comment" : "mac地址", "explain" : []},
            {"name" : "deviceType", "type" : "bst", "comment" : "设备类型", "explain" : []}
        ],
        "10003" : [
            {"name" : "name", "type" : "bst", "comment" : "账户名", "explain" : []}
        ]
    },
    "write" : {
        "10000" : [],
        "10001" : [
            {"name" : "result", "type" : "u8", "comment" : "结果", "explain" : []}
        ],
        "10002" : [
            {"name" : "result", "type" : "u8", "comment" : "结果", "explain" : []}
        ],
        "10003" : [
            {"name" : "name", "type" : "bst", "comment" : "角色名", "explain" : []}
        ]
    }
};