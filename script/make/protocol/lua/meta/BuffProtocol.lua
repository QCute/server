return {
    [11801] = {
        ["comment"] = "Buff列表",
        ["write"] = {name = "data", type = "map", comment = "", explain = {

        }},
        ["read"] = {name = "data", type = "list", comment = "Buff列表", explain = {
            {name = "data", type = "map", comment = "", explain = {
                {name = "buffId", type = "u32", comment = "BuffID", explain = {}},
                {name = "expireTime", type = "u32", comment = "结束时间", explain = {}},
                {name = "overlap", type = "u16", comment = "叠加数量", explain = {}}
            }}
        }}
    }
}