shopProtocol = {
    [11301] = {
        ["comment"] = "已购列表",
        ["write"] = {},
        ["read"] = {
            {name = "list", type = "list", comment = "已购买列表", explain = {
                {name = "shopId", type = "u32", comment = "商店ID", explain = {}},
                {name = "number", type = "u16", comment = "数量", explain = {}}
            }}
        }
    },
    [11302] = {
        ["comment"] = "购买",
        ["write"] = {
            {name = "shopId", type = "u32", comment = "商店ID", explain = {}},
            {name = "number", type = "u16", comment = "数量", explain = {}}
        },
        ["read"] = {
            {name = "result", type = "rst", comment = "结果", explain = {}}
        }
    }
}