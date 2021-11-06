local bubbleProtocol = {
    [12101] = {
        ["comment"] = "气泡列表",
        ["write"] = {},
        ["read"] = {
            {name = "list", type = "list", comment = "气泡列表", explain = {
                {name = "bubbleId", type = "u32", comment = "气泡ID", explain = {}},
                {name = "expireTime", type = "u32", comment = "过期时间", explain = {}}
            }}
        }
    },
    [12102] = {
        ["comment"] = "删除气泡",
        ["write"] = {},
        ["read"] = {
            {name = "list", type = "list", comment = "气泡ID列表", explain = {
                {name = "bubbleId", type = "u32", comment = "气泡ID", explain = {}}
            }}
        }
    }
}