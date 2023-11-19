return {
    [12101] = {
        ["comment"] = "气泡列表",
        ["write"] = {name = "data", type = "map", comment = "", explain = {

        }},
        ["read"] = {name = "data", type = "list", comment = "气泡列表", explain = {
            {name = "data", type = "map", comment = "", explain = {
                {name = "bubbleId", type = "u32", comment = "气泡ID", explain = {}},
                {name = "expireTime", type = "u32", comment = "过期时间", explain = {}}
            }}
        }}
    }
}