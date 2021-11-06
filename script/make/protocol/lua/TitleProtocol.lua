local titleProtocol = {
    [11901] = {
        ["comment"] = "称号列表",
        ["write"] = {},
        ["read"] = {
            {name = "list", type = "list", comment = "称号列表", explain = {
                {name = "titleId", type = "u32", comment = "称号ID", explain = {}},
                {name = "expireTime", type = "u32", comment = "过期时间", explain = {}}
            }}
        }
    },
    [11902] = {
        ["comment"] = "删除称号",
        ["write"] = {},
        ["read"] = {
            {name = "list", type = "list", comment = "称号ID列表", explain = {
                {name = "titleId", type = "u32", comment = "称号ID", explain = {}}
            }}
        }
    }
}