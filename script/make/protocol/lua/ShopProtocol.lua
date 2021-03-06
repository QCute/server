local shopProtocol = {
    ["write"] = {
        [11301] = {
            {name = "list", type = "list", comment = "已购买列表", explain = {
                {name = "shopId", type = "u32", comment = "商店ID", explain = {}},
                {name = "number", type = "u16", comment = "数量", explain = {}}
            }}
        },
        [11302] = {
            {name = "result", type = "rst", comment = "结果", explain = {}}
        }
    },
    ["read"] = {
        [11301] = {},
        [11302] = {
            {name = "shopId", type = "u32", comment = "商店ID", explain = {}},
            {name = "number", type = "u16", comment = "数量", explain = {}}
        }
    }
}