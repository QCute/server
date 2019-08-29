local roleProtocol = {
    ["read"] = {
        [10101] = {}
    },
    ["write"] = {
        [10101] = {
            {name = "roleId", type = "u64", comment = "角色ID", explain = {}},
            {name = "roleName", type = "bst", comment = "角色名", explain = {}},
            {name = "accountId", type = "bst", comment = "账号ID", explain = {}},
            {name = "accountName", type = "bst", comment = "账号名", explain = {}},
            {name = "sex", type = "u8", comment = "性别", explain = {}},
            {name = "level", type = "u64", comment = "等级", explain = {}},
            {name = "classes", type = "u8", comment = "职业", explain = {}},
            {name = "itemSize", type = "u16", comment = "普通背包大小", explain = {}},
            {name = "bagSize", type = "u16", comment = "装备背包大小", explain = {}},
            {name = "storeSize", type = "u16", comment = "仓库背包大小", explain = {}}
        }
    }
}