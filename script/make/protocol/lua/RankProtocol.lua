local rankProtocol = {
    ["read"] = {
        [19001] = {
            {name = "type", type = "u8", comment = "", explain = {}}
        }
    },
    ["write"] = {
        [19001] = {
            {name = "list", type = "list", comment = "排行榜", explain = {
                {name = "type", type = "u16", comment = "类型", explain = {}},
                {name = "rank", type = "u64", comment = "排名", explain = {}},
                {name = "key", type = "u64", comment = "键", explain = {}},
                {name = "value", type = "u64", comment = "值", explain = {}},
                {name = "time", type = "u32", comment = "时间", explain = {}},
                {name = "name", type = "bst", comment = "名字", explain = {}}
            }}
        }
    }
}