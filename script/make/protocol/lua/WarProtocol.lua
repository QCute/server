local warProtocol = {
    ["write"] = {
        [18001] = {
            {name = "result", type = "rst", comment = "结果", explain = {}}
        }
    },
    ["read"] = {
        [18001] = {
            {name = "monsterId", type = "u32", comment = "怪物Id", explain = {}}
        }
    }
}