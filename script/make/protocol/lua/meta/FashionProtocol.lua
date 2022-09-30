fashionProtocol = {
    [12001] = {
        ["comment"] = "时装列表",
        ["write"] = {},
        ["read"] = {
            {name = "list", type = "list", comment = "时装列表", explain = {
                {name = "fashionId", type = "u32", comment = "时装ID", explain = {}},
                {name = "expireTime", type = "u32", comment = "过期时间", explain = {}}
            }}
        }
    },
    [12002] = {
        ["comment"] = "删除时装",
        ["write"] = {},
        ["read"] = {
            {name = "list", type = "list", comment = "时装ID列表", explain = {
                {name = "fashionId", type = "u32", comment = "时装ID", explain = {}}
            }}
        }
    }
}