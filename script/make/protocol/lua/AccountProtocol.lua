local accountProtocol = {
    ["read"] = {
        [10000] = {},
        [10001] = {
            {name = "result", type = "u8", comment = "结果(1成功/2:服务器ID不对/3:没有找到账户/4:登录禁止/5:重复登录)", explain = {}}
        },
        [10002] = {
            {name = "result", type = "u8", comment = "结果(1:成功/2:长度不对/3:字符问题/4:敏感词/5:名字重复)", explain = {}}
        }
    },
    ["write"] = {
        [10000] = {},
        [10001] = {
            {name = "serverId", type = "u16", comment = "服务器ID", explain = {}},
            {name = "account", type = "bst", comment = "账户", explain = {}}
        },
        [10002] = {
            {name = "account", type = "bst", comment = "账户", explain = {}},
            {name = "roleName", type = "bst", comment = "角色名", explain = {}},
            {name = "serverId", type = "u16", comment = "服务器ID", explain = {}},
            {name = "sex", type = "u8", comment = "性别", explain = {}},
            {name = "classes", type = "u8", comment = "职业", explain = {}},
            {name = "channelId", type = "u16", comment = "渠道ID", explain = {}},
            {name = "deviceId", type = "bst", comment = "设备", explain = {}},
            {name = "mac", type = "bst", comment = "mac地址", explain = {}},
            {name = "deviceType", type = "bst", comment = "设备类型", explain = {}}
        }
    }
}