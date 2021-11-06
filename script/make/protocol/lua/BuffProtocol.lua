local buffProtocol = {
    [11801] = {
        ["comment"] = "Buff列表",
        ["write"] = {},
        ["read"] = {
            {name = "list", type = "list", comment = "Buff列表", explain = {
                {name = "buffId", type = "u32", comment = "BuffID", explain = {}},
                {name = "expireTime", type = "u32", comment = "结束时间", explain = {}},
                {name = "overlap", type = "u16", comment = "叠加数量", explain = {}}
            }}
        }
    },
    [11802] = {
        ["comment"] = "删除Buff列表",
        ["write"] = {},
        ["read"] = {
            {name = "list", type = "list", comment = "Buff列表", explain = {
                {name = "buffId", type = "u32", comment = "BuffID", explain = {}}
            }}
        }
    }
}